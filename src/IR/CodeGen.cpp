#include "IR/CodeGen.h"
#include <cassert>
#include <llvm/Support/raw_ostream.h>

// 循环上下文（用于处理break/continue）
struct LoopContext {
    llvm::BasicBlock* continueBB;
    llvm::BasicBlock* breakBB;
};

// 循环上下文栈
thread_local std::vector<LoopContext> loopContextStack;

void PushLoopContext(LoopContext ctx) {
    loopContextStack.push_back(ctx);
}

void PopLoopContext() {
    loopContextStack.pop_back();
}

// 获取当前循环上下文
LoopContext* GetCurrentLoopContext() {
    if (loopContextStack.empty()) return nullptr;
    return &loopContextStack.back();
}

// 进入新的作用域（新建一个符号表层）
void CodeGenerator::EnterScope() {
    symbolTables.emplace_back();  // 添加一个新的unordered_map作为当前作用域
}

// 离开当前作用域（移除最内层符号表）
void CodeGenerator::ExitScope() {
    if (!symbolTables.empty()) {
        symbolTables.pop_back();
    } else {
        llvm::errs() << "Warning: Trying to exit non-existent scope\n";
    }
}

// 添加符号到当前作用域
bool CodeGenerator::AddSymbol(const std::string& name, const SymbolInfo& info) {
    if (symbolTables.empty()) {
        enterScope();  // 确保至少有一个作用域
    }

    auto& currentScope = symbolTables.back();
    auto [iter, inserted] = currentScope.try_emplace(name, info);
    
    if (!inserted) {
        llvm::errs() << "Error: Symbol '" << name << "' already defined in current scope\n";
        return false;
    }
    return true;
}

// 查找符号（从内到外逐层查找）
SymbolInfo* CodeGenerator::LookupSymbol(const std::string& name) {
    // 反向遍历符号表（从最内层到最外层）
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;  // 返回符号信息的指针
        }
    }
    return nullptr;  // 未找到返回nullptr
}

CodeGenerator::CodeGenerator(){}
CodeGenerator::CodeGenerator(const std::string& moduleName)
    : Context(),
      Module(moduleName, Context),
      Builder(Context)
{
}

llvm::Value* CodeGenerator::EmitIfStmt(const IfStmt& ifStmt, llvm::Function* currentFunction) {
    // 1. 生成条件表达式的IR
    llvm::Value* condValue = EmitExpr(Builder, Context, Module, ifStmt.condition);
    
    // 2. 常量折叠优化
    if (llvm::ConstantInt* constCond = llvm::dyn_cast<llvm::ConstantInt>(condValue)) {
        // 条件为常量时的优化处理
        if (constCond->isOne()) {
            // 条件恒为真，只生成then分支
            return EmitStmt(Builder, Context, Module, ifStmt.then_branch, currentFunction);
        } else {
            // 条件恒为假
            if (ifStmt.else_branch) {
                // 有else分支则生成else分支
                return EmitStmt(Builder, Context, Module, ifStmt.else_branch, currentFunction);
            }
            // 没有else分支则不生成任何代码
            return nullptr;
        }
    }
    
    // 3. 非常量条件，生成完整控制流
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(Context, "if.then", currentFunction);
    llvm::BasicBlock* elseBB = ifStmt.else_branch ? 
        llvm::BasicBlock::Create(Context, "if.else", currentFunction) : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(Context, "if.end", currentFunction);
    
    // 生成条件分支指令
    Builder.CreateCondBr(condValue, thenBB, elseBB ? elseBB : mergeBB);
    
    // 生成then分支
    Builder.SetInsertPoint(thenBB);
    EmitStmt(Builder, Context, Module, ifStmt.then_branch, currentFunction);
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(mergeBB);
    }
    
    // 生成else分支（如果存在）
    if (elseBB) {
        Builder.SetInsertPoint(elseBB);
        EmitStmt(Builder, Context, Module, ifStmt.else_branch, currentFunction);
        if (!Builder.GetInsertBlock()->getTerminator()) {
            Builder.CreateBr(mergeBB);
        }
    }
    
    // 设置后续代码的插入点
    Builder.SetInsertPoint(mergeBB);
    
    return nullptr;
}

llvm::Value* CodeGenerator::EmitForStmt(const ForStmt& forStmt, llvm::Function* currentFunction) {
    // 生成初始化代码
    if (forStmt.init) {
        EmitStmt(Builder, Context, Module, forStmt.init, currentFunction);
    }

    // 创建基本块
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(Context, "for.cond", currentFunction);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(Context, "for.body", currentFunction);
    llvm::BasicBlock* incBB = llvm::BasicBlock::Create(Context, "for.inc", currentFunction);
    llvm::BasicBlock* endBB = llvm::BasicBlock::Create(Context, "for.end", currentFunction);

    // 跳转到条件判断
    Builder.CreateBr(condBB);

    // 条件判断部分
    Builder.SetInsertPoint(condBB);
    if (forStmt.cond) {
        llvm::Value* condValue = EmitExpr(Builder, Context, Module, forStmt.cond);
        Builder.CreateCondBr(condValue, bodyBB, endBB);
    } else {
        // 无条件循环
        Builder.CreateBr(bodyBB);
    }

    // 循环体部分
    Builder.SetInsertPoint(bodyBB);
    if (forStmt.body) {
        // 设置循环继续/中断的目标块
        LoopContext loopCtx = { incBB, endBB };
        PushLoopContext(loopCtx);
        
        EmitStmt(Builder, Context, Module, forStmt.body, currentFunction);
        
        PopLoopContext();
    }
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(incBB);
    }

    // 增量部分
    Builder.SetInsertPoint(incBB);
    if (forStmt.inc) {
        EmitExpr(Builder, Context, Module, forStmt.inc);
    }
    Builder.CreateBr(condBB);

    // 结束部分
    Builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitWhileStmt(const WhileStmt& whileStmt, llvm::Function* currentFunction) {
    // 创建基本块
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(Context, "while.cond", currentFunction);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(Context, "while.body", currentFunction);
    llvm::BasicBlock* endBB = llvm::BasicBlock::Create(Context, "while.end", currentFunction);

    // 跳转到条件判断
    Builder.CreateBr(condBB);

    // 条件判断部分
    Builder.SetInsertPoint(condBB);
    if (whileStmt.cond) {
        llvm::Value* condValue = EmitExpr(Builder, Context, Module, whileStmt.cond);
        Builder.CreateCondBr(condValue, bodyBB, endBB);
    } else {
        // 无条件循环
        Builder.CreateBr(bodyBB);
    }

    // 循环体部分
    Builder.SetInsertPoint(bodyBB);
    if (whileStmt.body) {
        // 设置循环继续/中断的目标块
        LoopContext loopCtx = { condBB, endBB };
        PushLoopContext(loopCtx);
        
        EmitStmt(Builder, Context, Module, whileStmt.body, currentFunction);
        
        PopLoopContext();
    }
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(condBB);
    }

    // 结束部分
    Builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitExprStmt(const SimpleStmt& exprStmt, llvm::Function* currentFunction) {
    if (exprStmt.expr) {
        // 生成表达式值，但忽略返回值（除非是块中最后一个表达式）
        return EmitExpr(Builder, Context, Module, exprStmt.expr);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitImportStmt(const Name& importStmt, llvm::Function* currentFunction) {
    // 导入语句主要在语义分析阶段处理
    // 这里可以生成必要的模块初始化代码
    std::string moduleName = importStmt.name;
    
    // 示例：生成对__import_<module>的调用
    llvm::FunctionType* importFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(Context), false);
    llvm::Function* importFunc = Module->getFunction("__import_" + moduleName);
    if (!importFunc) {
        importFunc = llvm::Function::Create(
            importFuncType, llvm::Function::ExternalLinkage,
            "__import_" + moduleName, Module);
    }
    
    Builder.CreateCall(importFunc);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitExportStmt(const Name& exportStmt, llvm::Function* currentFunction) {
    // 设置符号的链接属性
    if (llvm::GlobalValue* gv = Module->getNamedValue(exportStmt.name)) {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage);
        gv->setDSOLocal(true);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitReturnStmt(const SimpleStmt& returnStmt, llvm::Function* currentFunction) {
    if (returnStmt.expr) {
        llvm::Value* retVal = EmitExpr(Builder, Context, Module, returnStmt.expr);
        // 检查返回值类型是否匹配函数返回类型
        if (retVal->getType() != currentFunction->getReturnType()) {
            throw CodegenError("Return type mismatch in function " + 
                             currentFunction->getName().str());
        }
        Builder.CreateRet(retVal);
    } else {
        // 检查函数是否确实返回void
        if (!currentFunction->getReturnType()->isVoidTy()) {
            throw CodegenError("Void return in non-void function " +
                             currentFunction->getName().str());
        }
        Builder.CreateRetVoid();
    }
    return nullptr; // return语句不会继续执行后续代码
}

llvm::Value* CodeGenerator::EmitBlockStmt(const StmtSequence& blockStmt, llvm::Function* currentFunction) {
    // 创建新的作用域
    SymbolTableScope scope(SymbolTable);
    
    llvm::Value* lastValue = nullptr;
    
    // 生成块内所有语句
    for (const ASTNode* stmt : blockStmt.stmts) {
        lastValue = EmitStmt(Builder, Context, Module, stmt, currentFunction);
        
        // 如果遇到终止语句（如return），提前结束
        if (Builder.GetInsertBlock()->getTerminator()) {
            break;
        }
    }
    
    // 返回最后一个表达式的值（如果是表达式块）
    return lastValue;
}

llvm::Value* CodeGenerator::EmitBreakStmt() {
    if (auto* ctx = GetCurrentLoopContext()) {
        Builder.CreateBr(ctx->breakBB);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitContinueStmt() {
    if (auto* ctx = GetCurrentLoopContext()) {
        Builder.CreateBr(ctx->continueBB);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitStmt(ASTNode* stmt, llvm::Function* currentFunction) {
    if (!stmt) return nullptr;

    switch (stmt->type) {
        case AST_IF_STMT:
            return EmitIfStmt(Builder, Context, Module, stmt->if_stmt, currentFunction);
        
        case AST_FOR_STMT:
            return EmitForStmt(Builder, Context, Module, stmt->for_stmt, currentFunction);
        
        case AST_WHILE_STMT:
            return EmitWhileStmt(Builder, Context, Module, stmt->while_stmt, currentFunction);
        
        case AST_EXPR_STMT:
            return EmitExprStmt(Builder, Context, Module, stmt->expr_stmt, currentFunction);
        
        case AST_IMPORT_STMT:
            return EmitImportStmt(Builder, Context, Module, stmt->import_stmt, currentFunction);
        
        case AST_EXPORT_STMT:
            return EmitExportStmt(Builder, Context, Module, stmt->export_stmt, currentFunction);
        
        case AST_RETURN_STMT:
            return EmitReturnStmt(Builder, Context, Module, stmt->return_stmt, currentFunction);
        
        case AST_BLOCK_STMT:
            return EmitBlockStmt(Builder, Context, Module, stmt->block_stmt, currentFunction);
        
        case AST_VAR_DECL:
            return EmitVarDecl(Builder, Context, Module, stmt->var_decl, currentFunction);
        
        case AST_FUNCTION_DECL:
            return EmitFunctionDecl(Builder, Context, Module, stmt->func_decl);
        
        case AST_BREAK_STMT:
            return EmitBreakStmt(Builder, Context, Module, stmt->line, currentFunction);
        
        case AST_CONTINUE_STMT:
            return EmitContinueStmt(Builder, Context, Module, stmt->line, currentFunction);

        default:
            throw CodegenError("Unknown statement type at line " + std::to_string(stmt->line));
    }
}

llvm::Value* CodeGenerator::EmitLiteralExpr(TypeKind type, const void *value) 
{
  // 如果未传入 Module，尝试从 Builder 获取
  if (!Module && Builder.GetInsertBlock()) {
    Module = Builder.GetInsertBlock()->getModule();
  }
  switch (type) {
    // 整数类型
    case TYPE_INT8:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(int8_t*)value, true));
    case TYPE_INT16:
      return llvm::ConstantInt::get(Context, llvm::APInt(16, *(int16_t*)value, true));
    case TYPE_INT32:
      return llvm::ConstantInt::get(Context, llvm::APInt(32, *(int32_t*)value, true));
    case TYPE_INT64:
      return llvm::ConstantInt::get(Context, llvm::APInt(64, *(int64_t*)value, true));

    // 无符号整数
    case TYPE_UINT8:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(uint8_t*)value, false));
    case TYPE_UINT16:
      return llvm::ConstantInt::get(Context, llvm::APInt(16, *(uint16_t*)value, false));
    case TYPE_UINT32:
      return llvm::ConstantInt::get(Context, llvm::APInt(32, *(uint32_t*)value, false));
    case TYPE_UINT64:
      return llvm::ConstantInt::get(Context, llvm::APInt(64, *(uint64_t*)value, false));

    // 浮点数
    case TYPE_FLOAT16:
      return llvm::ConstantFP::get(Context, llvm::APFloat(llvm::APFloat::IEEEhalf(), *(uint16_t*)value));
    case TYPE_FLOAT32:
      return llvm::ConstantFP::get(Context, llvm::APFloat(*(float*)value));
    case TYPE_FLOAT64:
      return llvm::ConstantFP::get(Context, llvm::APFloat(*(double*)value));

    // 字符串
    case TYPE_STRING: {
      const char *str = (const char*)value;
      llvm::Constant *StrConst = llvm::ConstantDataArray::getString(Context, str);
      
      if (!Module) {
        // 如果没有 Module，无法创建全局变量，返回 nullptr 或报错
        return nullptr;
      }

      llvm::GlobalVariable *GV = new llvm::GlobalVariable(
          *Module,
          StrConst->getType(),
          true,
          llvm::GlobalValue::PrivateLinkage,
          StrConst,
          "str.literal");

      if (Builder.GetInsertBlock()) {
        // 如果有插入点，生成 GEP 指令
        return Builder.CreateInBoundsGEP(
            GV->getValueType(),
            GV,
            {Builder.getInt32(0), Builder.getInt32(0)});
      } else {
        // 全局初始化，直接返回全局变量
        return GV;
      }
    }

    // 字符
    case TYPE_CHAR:
      return llvm::ConstantInt::get(Context, llvm::APInt(8, *(char*)value, false));

    // 布尔值
    case TYPE_BOOLEAN:
      return llvm::ConstantInt::get(Context, llvm::APInt(1, *(bool*)value, false));

    // NULL（指针）
    case TYPE_PTR:
      return llvm::ConstantPointerNull::get(Builder.getPtrTy());

    default:
      return nullptr;  // 其他
  }
}

llvm::Value* CodeGenerator::EmitIdentifierExpr(ASTNode* expr) {
    llvm::Value* var = LookupSymbol(expr->identifier_expr.name);
    if (!var) throw CodegenError("Undefined variable: " + expr->identifier_expr.name.name);
    return Builder.CreateLoad(getLLVMType(expr->inferred_type), var, expr->identifier_expr.name);
}

llvm::Value* CodeGenerator::EmitBinaryExpr(ASTNode* expr) {
    const BinaryExpr& bin = expr->binary_expr;
    llvm::Value* L = EmitExpr(Builder, Context, Module, bin.left);
    llvm::Value* R = EmitExpr(Builder, Context, Module, bin.right);
    
    // 常量折叠优化
    if (llvm::Constant* LC = llvm::dyn_cast<llvm::Constant>(L))
        if (llvm::Constant* RC = llvm::dyn_cast<llvm::Constant>(R))
            if (llvm::Constant* folded = ConstantFoldBinaryOp(bin.op, LC, RC, expr->inferred_type))
                return folded;

    switch (bin.op) {
        case OP_ADD: return createArithOp(Builder, L, R, expr->inferred_type, true);
        case OP_SUB: return createArithOp(Builder, L, R, expr->inferred_type, false);
        case OP_MUL: return createMulOp(Builder, L, R, expr->inferred_type);
        case OP_DIV: return createDivOp(Builder, L, R, expr->inferred_type);
        case OP_LT: case OP_LE: case OP_GT: case OP_GE: case OP_EQ: case OP_NE:
            return createCmpOp(Builder, bin.op, L, R, expr->inferred_type);
        case OP_AND: return Builder.CreateAnd(L, R, "andtmp");
        case OP_OR:  return Builder.CreateOr(L, R, "ortmp");
        default:
            throw CodegenError("Unknown binary operator at line " + std::to_string(expr->line));
    }
}

llvm::Value* CodeGenerator::EmitAssignmentExpr(const ASTNode* expr) {
    const BinaryExpr& assign = expr->assignment_expr;
    if (!assign.left || assign.left->type != AST_IDENTIFIER_EXPR)
        throw CodegenError("Invalid assignment target at line " + std::to_string(expr->line));

    llvm::Value* rhs = EmitExpr(Builder, Context, Module, assign.right);
    llvm::Value* var = LookupSymbol(assign.left->identifier_expr.name);
    if (!var) throw CodegenError("Undefined variable: " + assign.left->identifier_expr.name);
    
    Builder.CreateStore(rhs, var);
    return rhs;
}

llvm::Value* CodeGenerator::EmitCallExpr(const ASTNode* expr) {
    const CallExpr& call = expr->call_expr;
    llvm::Function* callee = Module->getFunction(call.func_name);
    if (!callee)
        throw CodegenError("Unknown function: " + call.func_name + " at line " + std::to_string(expr->line));

    std::vector<llvm::Value*> args;
    for (const ASTNode* arg : call.args) {
        args.push_back(EmitExpr(Builder, Context, Module, arg));
        if (!args.back()) return nullptr;
    }

    if (callee->arg_size() != args.size())
        throw CodegenError("Argument count mismatch for " + call.func_name);

    return Builder.CreateCall(callee, args, "calltmp");
}

llvm::Value* CodeGenerator::EmitArrayAccessExpr(const ASTNode* expr) {
    const ArrayAccessExpr& arr = expr->array_access_expr;
    llvm::Value* array = EmitExpr(Builder, Context, Module, arr.array);
    llvm::Value* index = EmitExpr(Builder, Context, Module, arr.index);
    
    if (EnableBoundsChecking) {
        emitBoundsCheck(Builder, array, index, expr->line);
    }

    return Builder.CreateInBoundsGEP(
        getLLVMType(expr->inferred_type), array, index, "arrayidx");
}

llvm::Value* CodeGenerator::EmitObjectAccessExpr(const ASTNode* expr) {
    const ObjectAccessExpr& obj = expr->object_access_expr;
    llvm::Value* object = EmitExpr(Builder, Context, Module, obj.object);
    
    if (!object->getType()->isPointerTy() || 
        !object->getType()->getPointerElementType()->isStructTy()) {
        throw CodegenError("Invalid object access at line " + std::to_string(expr->line));
    }

    llvm::StructType* structTy = llvm::cast<llvm::StructType>(
        object->getType()->getPointerElementType());
    
    int memberIdx = getStructMemberIndex(structTy, obj.member_name);
    if (memberIdx == -1) {
        throw CodegenError("Unknown member '" + obj.member_name + 
                          "' at line " + std::to_string(expr->line));
    }

    return Builder.CreateStructGEP(structTy, object, memberIdx, obj.member_name + ".ptr");
}

llvm::Value* CodeGenerator::EmitAnonymousFuncExpr(const ASTNode* expr) {
    const AnoymousFuncExpr& anon = expr->anonymous_func_expr;
    
    llvm::FunctionType* funcType = llvm::FunctionType::get(
        getLLVMType(anon.return_type), 
        getLLVMParamTypes(anon.parameters), 
        false);
    
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::PrivateLinkage, 
        "lambda", Module);
    
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        arg.setName(anon.parameters[idx++].name);
    }
    
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(Context, "entry", func);
    Builder.SetInsertPoint(entry);
    
    SymbolTableScope scope(SymbolTable);
    
    idx = 0;
    for (auto& arg : func->args()) {
        SymbolTable.add(anon.parameters[idx++].name, &arg);
    }
    
    llvm::Value* bodyVal = EmitStmt(Builder, Context, Module, anon.body, func);
    
    if (!Builder.GetInsertBlock()->getTerminator()) {
        if (anon.return_type == VALUE_VOID) {
            Builder.CreateRetVoid();
        } else {
            Builder.CreateRet(bodyVal);
        }
    }
    
    return func;
}

llvm::Value* CodeGenerator::EmitExpr(const ASTNode* expr) {
    if (!expr) return nullptr;

    switch (expr->type) {
        case AST_LITERAL_EXPR:    return EmitLiteralExpr(Builder, Context, Module, expr);
        case AST_IDENTIFIER_EXPR: return EmitIdentifierExpr(Builder, Context, Module, expr);
        case AST_BINARY_EXPR:     return EmitBinaryExpr(Builder, Context, Module, expr);
        case AST_ASSIGNMENT_EXPR: return EmitAssignmentExpr(Builder, Context, Module, expr);
        case AST_CALL_EXPR:       return EmitCallExpr(Builder, Context, Module, expr);
        case AST_ARRAY_ACCESS_EXPR: return EmitArrayAccessExpr(Builder, Context, Module, expr);
        case AST_OBJECT_ACCESS_EXPR: return EmitObjectAccessExpr(Builder, Context, Module, expr);
        case AST_ANONYMOUS_FUNC_EXPR: return EmitAnonymousFuncExpr(Builder, Context, Module, expr);
        default: throw CodegenError("Unsupported expression type at line " + std::to_string(expr->line));
    }
}

llvm::Value* CodeGenerator::EmitVarDecl(ASTNode *node) {
    if (!node) return nullptr;
    assert(node->type == AST_VAR_DECL && "node->type != AST_VAR_DECL");

    VarDecl decl = node->var_decl;
    // 1. 根据字面量类型获取LLVM类型, 没有value先不生成
    ASTNode *value = decl.value;
    if (value == nullptr) return nullptr;

    llvm::Type* ty = ConvertToLLVMType(Context, Builder, node->inferred_type);
    if (ty == nullptr)
    {
        std::cerr << "Unsupported or complex type for variable: " << node->inferred_type << "\n";
        return nullptr;
    }
    
    assert(decl.name.name != nullptr && "decl.name.name is null");

    // 2. 创建alloca指令（在栈上分配空间）
    llvm::Value* alloc = Builder.CreateAlloca(ty, nullptr, decl.name.name);

    // 3. 处理初始化值
    llvm::Value* initVal = EmitExpr(Builder, Context, Module, value);
    Builder.CreateStore(initVal, alloc);

    // 4. 如果是常量，标记为不可修改
    if (decl.var_type == VAR_TYPE_CONSTANT) {
        if (llvm::GlobalVariable* GV = llvm::dyn_cast<llvm::GlobalVariable>(alloc)) {
            GV->setConstant(true);
        }
    }

    return alloc;
}

llvm::Function* CodeGenerator::EmitFunctionDecl(ASTNode *node) {
    if (!node) return nullptr;
    assert(node->type == AST_FUNC_DECL && "node->type != AST_FUNC_DECL");

    FunctionDecl funcDecl = node->func_decl;                                    
    // 1. 转换返回类型
    llvm::Type* retTy = ConvertToLLVMType(Context, Builder, node->inferred_type);
    if (!retTy) {
        std::cerr << "Unsupported function return type: " << node->inferred_type << "\n";
        return nullptr;
    }

    // 2. 转换参数类型
    std::vector<llvm::Type*> paramTypes;
    for (Parameter* param = funcDecl.params; param != nullptr; param = param->next) {
        llvm::Type* ty = ConvertToLLVMType(Context, Builder, param->inferred_type);
        if (!ty) {
            std::cerr << "Unsupported function parameter type: " << param->inferred_type << "\n";
            return nullptr;
        }
        paramTypes.push_back(param->isReference ? ty->getPointerTo() : ty);
    }

    // 3. 创建函数类型
    llvm::FunctionType* funcType = llvm::FunctionType::get(retTy, paramTypes, false);

    // 4. 创建函数对象
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcDecl.name.name, Module);

    // 5. 设置参数名称
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        Parameter* param = funcDecl.params;
        for (int i = 0; i < idx && param != nullptr; ++i) {
            param = param->next;
        }
        if (param) {
            arg.setName(param->name.name);
        }
    }

    // 6. 如果函数有实现（非声明），生成函数体
    if (funcDecl.body) {
        // 创建入口基本块
        llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(Context, "entry", func);
        Builder.SetInsertPoint(entryBB);

        // 处理函数参数：在栈上分配空间并存储初始值
        idx = 0;
        for (auto& arg : func->args()) {
            Parameter* param = funcDecl.params;
            for (int i = 0; i < idx && param != nullptr; ++i) {
                param = param->next;
            }

            llvm::AllocaInst* alloca = Builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
            Builder.CreateStore(&arg, alloca);
            idx++;
        }

        // 生成函数体
        try {
            EmitStmt(Builder, Context, Module, funcDecl.body, func);

            // 确保函数有终止指令
            if (!Builder.GetInsertBlock()->getTerminator()) {
                if (retTy->isVoidTy()) {
                    Builder.CreateRetVoid();
                } else {
                    throw CodegenError("Non-void function missing return statement");
                }
            }
        } catch (const CodegenError& e) {
            // 发生错误时删除问题函数
            func->eraseFromParent();
            throw;
        }
    }

    return func;
}

llvm::Type* CodeGenerator::EmitStructOrUnionDecl(ASTNode *node, bool isPacked = false) {
                                    
    if (!node) return nullptr;
    assert((node->type == AST_STRUCT_DECL || node->type == AST_UNION_DECL) && "node->type != AST_STRUCT_OR_UNION_DECL");
    StructOrUnionDecl decl = node->struct_or_union_decl;

    // 1. 检查是否已生成该类型（防止递归无限循环）
    if (decl.name.name) {
        if (auto* existingType = Module->getTypeByName(decl.name.name)) {
            return existingType;
        }
    }

    // 2. 创建不完整类型并注册（处理递归类型）
    llvm::StructType* structType = llvm::StructType::create(Context, decl.name.name);
    if (decl.name.name) {
        Module->getOrInsertType(decl.name.name, structType);
    }

    // 3. 收集成员类型
    std::vector<llvm::Type*> memberTypes;
    std::vector<std::string> memberNames;
    
    for (const MemberList* member = decl.members; member != nullptr; member = member->next) {
        ASTNode *member_decl = member->decl;
        if (!member_decl) continue;

        switch (member_decl->type) {
            case AST_MEMBER_DECL: {
                // 普通成员变量
                llvm::Type* memberType = getLLVMType(Context, member_decl->inferred_type);
                memberTypes.push_back(memberType);
                memberNames.push_back(member_decl->member_decl.name);
                break;
            }
            case AST_STRUCT_DECL: 
            case AST_UNION_DECL: 
            {
                // 嵌套匿名结构体/联合体
                StructOrUnionDecl nested = member_decl->struct_or_union_decl;
                llvm::Type* nestedType = EmitStructOrUnionDecl(Context, Module, member_decl);
                memberTypes.push_back(nestedType);
                memberNames.push_back(nested.name.name ?  nested.name.name : "anon");
                break;
            }
            default:
                throw CodegenError("Unsupported member type in struct/union");
        }
    }

    // 4. 设置结构体主体
    structType->setBody(memberTypes, isPacked);
    return structType;
}

llvm::Value* CodeGenerator::EmitDecl(ASTNode* decl, llvm::Function* currentFunction = nullptr) {
    if (!decl) return nullptr;

    switch (decl->type) {
        case AST_VAR_DECL: {
            // 处理变量声明（全局/局部）
            return EmitVarDecl(Builder, Context, Module, decl, currentFunction);
        }
        case AST_FUNCTION_DECL: {
            // 处理函数声明/定义
            return EmitFunctionDecl(Builder, Context, Module, decl);
        }
        case AST_STRUCT_DECL: 
        case AST_UNION_DECL: {
            // 处理结构体/联合体定义
            return EmitStructOrUnionDecl(Context, Module, decl);
        }
        default: {
            // 处理可执行语句（当currentFunction为nullptr时生成全局初始化代码）
            return EmitStmt(Builder, Context, Module, decl, currentFunction);
        }
    }
}

// 常量折叠优化
llvm::Constant* CodeGenerator::ConstantFoldBinaryOp(Operator op, llvm::Constant* L, llvm::Constant* R, TypeKind type) {
    if (type == TYPE_FLOAT32) {
        auto* LF = llvm::cast<llvm::ConstantFP>(L);
        auto* RF = llvm::cast<llvm::ConstantFP>(R);
        double lv = LF->getValueAPF().convertToDouble();
        double rv = RF->getValueAPF().convertToDouble();
        
        switch (op) {
            case OP_PLUS: return llvm::ConstantFP::get(L->getContext(), lv + rv);
            case OP_MINUS: return llvm::ConstantFP::get(L->getContext(), lv - rv);
            case OP_STAR: return llvm::ConstantFP::get(L->getContext(), lv * rv);
            case OP_SLASH: return rv != 0 ? llvm::ConstantFP::get(L->getContext(), lv / rv) : nullptr;
            case OP_LT:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv < rv));
            case OP_LE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv <= rv));
            case OP_GT:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv > rv));
            case OP_GE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv >= rv));
            case OP_EQ:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv == rv));
            case OP_NE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv != rv));
            default: return nullptr;
        }
    } else if (type == TYPE_INT32 || type == TYPE_BOOLEAN) {
        auto* LI = llvm::cast<llvm::ConstantInt>(L);
        auto* RI = llvm::cast<llvm::ConstantInt>(R);
        int64_t lv = LI->getSExtValue();
        int64_t rv = RI->getSExtValue();
        
        switch (op) {
            case OP_PLUS: return llvm::ConstantInt::get(L->getContext(), llvm::APInt(32, lv + rv, true));
            case OP_MINUS: return llvm::ConstantInt::get(L->getContext(), llvm::APInt(32, lv - rv, true));
            case OP_STAR: return llvm::ConstantInt::get(L->getContext(), llvm::APInt(32, lv * rv, true));
            case OP_SLASH: return rv != 0 ? llvm::ConstantInt::get(L->getContext(), llvm::APInt(32, lv / rv, true)) : nullptr;
            case OP_LT:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv < rv));
            case OP_LE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv <= rv));
            case OP_GT:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv > rv));
            case OP_GE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv >= rv));
            case OP_EQ:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv == rv));
            case OP_NE:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv != rv));
            case OP_AND: return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv && rv));
            case OP_OR:  return llvm::ConstantInt::get(L->getContext(), llvm::APInt(1, lv || rv));
            default: return nullptr;
        }
    }
    return nullptr;
}

// 边界检查
void CodeGenerator::emitBoundsCheck(llvm::Value* array, llvm::Value* index, int line) {
    llvm::Function* failFunc = Builder.GetInsertBlock()->getParent()->getParent()->getFunction("__bounds_check_fail");
    if (!failFunc) {
        // 创建边界检查失败处理函数
        llvm::FunctionType* ft = llvm::FunctionType::get(
            llvm::Type::getVoidTy(Builder.getContext()), false);
        failFunc = llvm::Function::Create(
            ft, llvm::Function::ExternalLinkage,
            "__bounds_check_fail", Builder.GetInsertBlock()->getParent()->getParent());
    }

    llvm::BasicBlock* boundsOk = llvm::BasicBlock::Create(Builder.getContext(), "bounds.ok",
        Builder.GetInsertBlock()->getParent());
    llvm::BasicBlock* boundsFail = llvm::BasicBlock::Create(Builder.getContext(), "bounds.fail",
        Builder.GetInsertBlock()->getParent());

    // 获取数组长度（假设数组有length字段）
    llvm::Value* len = Builder.CreateStructGEP(array->getType()->getPointerElementType(), array, 0, "len.ptr");
    len = Builder.CreateLoad(len, "len");
    
    // 比较索引和长度
    llvm::Value* cmp = Builder.CreateICmpULT(index, len, "bounds.cmp");
    Builder.CreateCondBr(cmp, boundsOk, boundsFail);
    
    // 失败处理
    Builder.SetInsertPoint(boundsFail);
    Builder.CreateCall(failFunc);
    Builder.CreateUnreachable();
    
    // 继续正常流程
    Builder.SetInsertPoint(boundsOk);
}

llvm::Type* CodeGenerator::ConvertToLLVMType(TypeKind type) {
    switch (type) {
        // 有符号整型
        case TYPE_INT8:    return Builder.getInt8Ty();
        case TYPE_INT16:   return Builder.getInt16Ty();
        case TYPE_INT32:   return Builder.getInt32Ty();
        case TYPE_INT64:   return Builder.getInt64Ty();
        
        // 无符号整型（LLVM用相同类型表示，区别在于操作指令）
        case TYPE_UINT8:  return Builder.getInt8Ty();
        case TYPE_UINT16: return Builder.getInt16Ty();
        case TYPE_UINT32: return Builder.getInt32Ty();
        case TYPE_UINT64: return Builder.getInt64Ty();
        
        // 浮点型
        case TYPE_FLOAT16: return Builder.getHalfTy();
        case TYPE_FLOAT32: return Builder.getFloatTy();
        case TYPE_FLOAT64: return Builder.getDoubleTy();
        
        // 基本类型
        case TYPE_BOOLEAN: return Builder.getInt1Ty();
        case TYPE_CHAR:   return Builder.getInt8Ty();
        case TYPE_STRING:  return Builder.getInt8PtrTy();
        case TYPE_VOID:   return Builder.getVoidTy();
        case TYPE_PTR:     return Builder.getPtrTy();
        
        // 复合类型需要额外处理
        case TYPE_ARRAY:   return nullptr; // 需要额外提供元素类型和长度
        case TYPE_STRUCT:  return nullptr; // 需要提供字段类型列表
        case TYPE_UNION:   return nullptr; // 类似结构体但内存布局不同
        case TYPE_FUNCTION: return nullptr; // 需要提供函数签名
        
        case TYPE_UNKNOWN:
        case TYPE_ANY:
        default:
            llvm::errs() << "Unknown or unsupported type\n";
            return nullptr;
    }
}

llvm::Type* CodeGenerator::ConvertArrayType(llvm::Type* elementType, uint64_t length) {
    if (!elementType) return nullptr;
    return llvm::ArrayType::get(elementType, length);
}

llvm::Type* CodeGenerator::ConvertStructType( const std::vector<llvm::Type*>& fieldTypes,
                             const std::string& name = "", bool isPacked = false) {
    return llvm::StructType::create(Context, fieldTypes, name, isPacked);
}

llvm::FunctionType* CodeGenerator::ConvertFunctionType(llvm::Type* returnType,
                                      const std::vector<llvm::Type*>& paramTypes,
                                      bool isVarArg = false) {
    return llvm::FunctionType::get(returnType, paramTypes, isVarArg);
}

void CodeGenerator::EmitProgram(ASTNode *node) {
    if (!node) return nullptr;
    assert(node->type == AST_PROGRAM && "node->type != AST_PROGRAM");

    StmtSequence program = node->program;
    // 单次遍历处理所有语句
    for (StatementList* stmtNode = program.statements; stmtNode != nullptr; stmtNode = stmtNode->next) {
        if (!stmtNode->statement) continue;

        // 统一通过 EmitDecl 处理所有语句
        EmitDecl(Builder, Context, Module, stmtNode->statement);
    }

    // 验证模块完整性
    std::string verifyErrors;
    llvm::raw_string_ostream os(verifyErrors);
    if (llvm::verifyModule(*Module, &os)) {
        throw CodegenError("Module verification failed:\n" + os.str());
    }
    return
}

void CodeGenerator::dumpIR() const {
    Module.print(llvm::outs(), nullptr);
}

//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o ir
int main() {
    llvm::LLVMContext Context;
    llvm::Module Module("literal_demo", Context);
    llvm::IRBuilder<> Builder(Context);
    // ========================字面量测试===========================
    // 创建演示函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(Builder.getVoidTy(), false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "demo_literals", Module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(Context, "entry", func);
    Builder.SetInsertPoint(entry);

    // 1. 整型字面量
    int8_t i8 = -42;
    llvm::Value* li8 = EmitLiteral(Builder, Context, &Module, TYPE_INT8, &i8);
    llvm::Value* i8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "i8.addr");
    Builder.CreateStore(li8, i8_alloca);

    int16_t i16 = -32768;
    llvm::Value* li16 = EmitLiteral(Builder, Context, &Module, TYPE_INT16, &i16);
    llvm::Value* i16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "i16.addr");
    Builder.CreateStore(li16, i16_alloca);

    int32_t i32 = 2147483647;
    llvm::Value* li32 = EmitLiteral(Builder, Context, &Module, TYPE_INT32, &i32);
    llvm::Value* i32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "i32.addr");
    Builder.CreateStore(li32, i32_alloca);

    int64_t i64 = -9223372036854775807LL;
    llvm::Value* li64 = EmitLiteral(Builder, Context, &Module, TYPE_INT64, &i64);
    llvm::Value* i64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "i64.addr");
    Builder.CreateStore(li64, i64_alloca);

    // 2. 无符号整型字面量
    uint8_t u8 = 255;
    llvm::Value* lu8 = EmitLiteral(Builder, Context, &Module, TYPE_UINT8, &u8);
    llvm::Value* u8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "u8.addr");
    Builder.CreateStore(lu8, u8_alloca);

    uint16_t u16 = 65535;
    llvm::Value* lu16 = EmitLiteral(Builder, Context, &Module, TYPE_UINT16, &u16);
    llvm::Value* u16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "u16.addr");
    Builder.CreateStore(lu16, u16_alloca);

    uint32_t u32 = 4294967295;
    llvm::Value* lu32 = EmitLiteral(Builder, Context, &Module, TYPE_UINT32, &u32);
    llvm::Value* u32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "u32.addr");
    Builder.CreateStore(lu32, u32_alloca);

    uint64_t u64 = 18446744073709551615ULL;
    llvm::Value* lu64 = EmitLiteral(Builder, Context, &Module, TYPE_UINT64, &u64);
    llvm::Value* u64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "u64.addr");
    Builder.CreateStore(lu64, u64_alloca);

    // 3. 浮点数字面量
    uint16_t f16_bits = 0x3C00; // 1.0 in half precision
    llvm::Value* lf16 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT16, &f16_bits);
    llvm::Value* f16_alloca = Builder.CreateAlloca(Builder.getHalfTy(), nullptr, "f16.addr");
    Builder.CreateStore(lf16, f16_alloca);

    float f32 = 3.1415926f;
    llvm::Value* lf32 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT32, &f32);
    llvm::Value* f32_alloca = Builder.CreateAlloca(Builder.getFloatTy(), nullptr, "f32.addr");
    Builder.CreateStore(lf32, f32_alloca);

    double f64 = 2.718281828459045;
    llvm::Value* lf64 = EmitLiteral(Builder, Context, &Module, TYPE_FLOAT64, &f64);
    llvm::Value* f64_alloca = Builder.CreateAlloca(Builder.getDoubleTy(), nullptr, "f64.addr");
    Builder.CreateStore(lf64, f64_alloca);

    // 4. 字符串字面量
    const char* str = "Hello LLVM! 你好！";
    llvm::Value* lstr = EmitLiteral(Builder, Context, &Module, TYPE_STRING, str);
    llvm::Value* str_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "str.addr");
    Builder.CreateStore(lstr, str_alloca);

    // 5. 字符字面量
    char ch = 'A';
    llvm::Value* lch = EmitLiteral(Builder, Context, &Module, TYPE_CHAR, &ch);
    llvm::Value* ch_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "ch.addr");
    Builder.CreateStore(lch, ch_alloca);

    // 6. 布尔字面量
    bool b = true;
    llvm::Value* lbool = EmitLiteral(Builder, Context, &Module, TYPE_BOOLEAN, &b);
    llvm::Value* bool_alloca = Builder.CreateAlloca(Builder.getInt1Ty(), nullptr, "bool.addr");
    Builder.CreateStore(lbool, bool_alloca);

    // 7. NULL字面量
    llvm::Value* lnull = EmitLiteral(Builder, Context, &Module, TYPE_PTR, nullptr);
    llvm::Value* null_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "null.addr");
    Builder.CreateStore(lnull, null_alloca);

    // 函数返回
    Builder.CreateRetVoid();

    // 验证并打印模块
    if (llvm::verifyModule(Module, &llvm::errs())) {
    std::cerr << "Error verifying module!\n";
    return 1;
    }

    Module.print(llvm::outs(), nullptr);
    
    // ========================变量测试===========================
    llvm::LLVMContext Context1;
    llvm::Module Module1("var_decl_demo", Context1);
    llvm::IRBuilder<> Builder1(Context1);

    // 创建示例函数
    llvm::FunctionType* funcType1 = llvm::FunctionType::get(Builder1.getVoidTy(), false);
    llvm::Function* func1 = llvm::Function::Create(funcType1, llvm::Function::ExternalLinkage, "test_vars", Module1);
    llvm::BasicBlock* entry1 = llvm::BasicBlock::Create(Context1, "entry", func1);
    Builder1.SetInsertPoint(entry1);

    // 示例1：声明可修改的int32变量
    int32_t varValue1 = 42;
    VarDecl varDecl1 = {"myVar", VAR_TYPE_VARIABLE, TYPE_INT32, &varValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl1);

    // 示例2：声明浮点常量
    float constValue1 = 3.14f;
    VarDecl varDecl2 = {"PI", VAR_TYPE_CONSTANT, TYPE_FLOAT32, &constValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl2);

    // 示例3：声明字符串变量
    const char* strValue1 = "Hello";
    VarDecl varDecl3 = {"greeting", VAR_TYPE_VARIABLE, TYPE_STRING, strValue1};
    EmitVarDecl(Builder1, Context1, &Module1, varDecl3);

    Builder1.CreateRetVoid();

    // 验证并打印
    if (llvm::verifyModule(Module1, &llvm::errs())) {
        std::cerr << "Error verifying module!\n";
        return 1;
    }

    Module1.print(llvm::outs(), nullptr);

    // ========================函数测试===========================
    llvm::LLVMContext Context2;
    llvm::Module Module2("func_decl_demo", Context2);
    llvm::IRBuilder<> Builder2(Context2);

    // 示例1：声明 int foo(int a, float& b)
    FunctionDecl func21;
    func21.name = "foo";
    func21.returnType = TYPE_INT32;
    func21.params = {
        {"a", TYPE_INT32, false},
        {"b", TYPE_FLOAT32, true}
    };
    llvm::Function* fooFunc = EmitFunctionDecl(Builder2, Context2, &Module2, func21);

    // 示例2：声明 void bar(...)
    FunctionDecl func22;
    func22.name = "bar";
    func22.returnType = TYPE_VOID;
    func22.isVarArg = true;
    llvm::Function* barFunc = EmitFunctionDecl(Builder2, Context2, &Module2, func22);

    // 打印生成的IR
    Module2.print(llvm::outs(), nullptr);
    return 0;
}
