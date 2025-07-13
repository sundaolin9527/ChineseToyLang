#include <cassert>
#include <stdexcept>
#include <cstdlib>
#include "IR/CodeGen.h"
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
        EnterScope();  // 确保至少有一个作用域
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
CodeGenerator::SymbolInfo* CodeGenerator::LookupSymbol(const std::string& name) {
    // 反向遍历符号表（从最内层到最外层）
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;  // 返回符号信息的指针
        }
    }
    return nullptr;  // 未找到返回nullptr
}

CodeGenerator::CodeGenerator(const std::string& moduleName)
    : Context(),
      Module(std::make_unique<llvm::Module>(moduleName, Context)),
      Builder(Context)
{
}

llvm::Value* CodeGenerator::EmitIfStmt(IfStmt& ifStmt, llvm::Function* currentFunction) {
    // 1. 生成条件表达式的IR
    llvm::Value* condValue = EmitExpr(ifStmt.condition);
    
    // 2. 常量折叠优化
    if (llvm::ConstantInt* constCond = llvm::dyn_cast<llvm::ConstantInt>(condValue)) {
        // 条件为常量时的优化处理
        if (constCond->isOne()) {
            // 条件恒为真，只生成then分支
            return EmitStmt(ifStmt.then_branch, currentFunction);
        } else {
            // 条件恒为假
            if (ifStmt.else_branch) {
                // 有else分支则生成else分支
                return EmitStmt(ifStmt.else_branch, currentFunction);
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
    EmitStmt(ifStmt.then_branch, currentFunction);
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(mergeBB);
    }
    
    // 生成else分支（如果存在）
    if (elseBB) {
        Builder.SetInsertPoint(elseBB);
        EmitStmt(ifStmt.else_branch, currentFunction);
        if (!Builder.GetInsertBlock()->getTerminator()) {
            Builder.CreateBr(mergeBB);
        }
    }
    
    // 设置后续代码的插入点
    Builder.SetInsertPoint(mergeBB);
    
    return nullptr;
}

llvm::Value* CodeGenerator::EmitForStmt(ForStmt& forStmt, llvm::Function* currentFunction) {
    // 生成初始化代码
    if (forStmt.init) {
        EmitStmt(forStmt.init, currentFunction);
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
    if (forStmt.condition) {
        llvm::Value* condValue = EmitExpr(forStmt.condition);
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
        
        EmitStmt(forStmt.body, currentFunction);
        
        PopLoopContext();
    }
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(incBB);
    }

    // 增量部分
    Builder.SetInsertPoint(incBB);
    if (forStmt.increase) {
        EmitExpr(forStmt.increase);
    }
    Builder.CreateBr(condBB);

    // 结束部分
    Builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitWhileStmt(WhileStmt& whileStmt, llvm::Function* currentFunction) {
    // 创建基本块
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(Context, "while.cond", currentFunction);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(Context, "while.body", currentFunction);
    llvm::BasicBlock* endBB = llvm::BasicBlock::Create(Context, "while.end", currentFunction);

    // 跳转到条件判断
    Builder.CreateBr(condBB);

    // 条件判断部分
    Builder.SetInsertPoint(condBB);
    if (whileStmt.condition) {
        llvm::Value* condValue = EmitExpr(whileStmt.condition);
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
        
        EmitStmt(whileStmt.body, currentFunction);
        
        PopLoopContext();
    }
    if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(condBB);
    }

    // 结束部分
    Builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitExprStmt(SimpleStmt& exprStmt, llvm::Function* currentFunction) {
    if (exprStmt.expression) {
        // 生成表达式值，但忽略返回值（除非是块中最后一个表达式）
        return EmitExpr(exprStmt.expression);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitImportStmt(Name& importStmt, llvm::Function* currentFunction) {
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
            "__import_" + moduleName, *Module);
    }
    
    Builder.CreateCall(importFunc);
    return nullptr;
}

llvm::Value* CodeGenerator::EmitExportStmt(Name& exportStmt, llvm::Function* currentFunction) {
    // 设置符号的链接属性
    if (llvm::GlobalValue* gv = Module->getNamedValue(exportStmt.name)) {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage);
        gv->setDSOLocal(true);
    }
    return nullptr;
}

llvm::Value* CodeGenerator::EmitReturnStmt(SimpleStmt& returnStmt, llvm::Function* currentFunction) {
    if (returnStmt.expression) {
        llvm::Value* retVal = EmitExpr(returnStmt.expression);
        // 检查返回值类型是否匹配函数返回类型
        if (retVal->getType() != currentFunction->getReturnType()) {
            throw std::runtime_error("Return type mismatch in function " + 
                             currentFunction->getName().str());
        }
        Builder.CreateRet(retVal);
    } else {
        // 检查函数是否确实返回void
        if (!currentFunction->getReturnType()->isVoidTy()) {
            throw std::runtime_error("Void return in non-void function " +
                             currentFunction->getName().str());
        }
        Builder.CreateRetVoid();
    }
    return nullptr; // return语句不会继续执行后续代码
}

llvm::Value* CodeGenerator::EmitBlockStmt(StmtSequence& blockStmt, llvm::Function* currentFunction) {
    // 创建新的作用域
    EnterScope();
    
    llvm::Value* lastValue = nullptr;
    
    // 生成块内所有语句
    for (StatementList* stmt = blockStmt.statements; stmt != nullptr; stmt = stmt->next) {
        lastValue = EmitStmt(stmt->statement, currentFunction);
        
        // 如果遇到终止语句（如return），提前结束
        if (Builder.GetInsertBlock()->getTerminator()) {
            break;
        }
    }
    ExitScope();
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
            return EmitIfStmt(stmt->if_stmt, currentFunction);
        
        case AST_FOR_STMT:
            return EmitForStmt(stmt->for_stmt, currentFunction);
        
        case AST_WHILE_STMT:
            return EmitWhileStmt(stmt->while_stmt, currentFunction);
        
        case AST_EXPR_STMT:
            return EmitExprStmt(stmt->expr_stmt, currentFunction);
        
        case AST_IMPORT_STMT:
            return EmitImportStmt(stmt->import_stmt, currentFunction);
        
        case AST_EXPORT_STMT:
            return EmitExportStmt(stmt->export_stmt, currentFunction);
        
        case AST_RETURN_STMT:
            return EmitReturnStmt(stmt->return_stmt, currentFunction);
        
        case AST_BLOCK_STMT:
            return EmitBlockStmt(stmt->block_stmt, currentFunction);
        
        case AST_VAR_DECL:
            return EmitVarDecl(stmt);
        
        case AST_FUNC_DECL:
            return EmitFunctionDecl(stmt);
        
        case AST_BREAK_STMT:
            return EmitBreakStmt();
        
        case AST_CONTINUE_STMT:
            return EmitContinueStmt();

        default:
            throw std::runtime_error("Unknown statement type at line " + std::to_string(stmt->line));
    }
}

template<typename T>
llvm::Value* CreateIntegerConstant(llvm::LLVMContext& Context, const std::string& str, bool isSigned) {
    T value;
    if constexpr (std::is_same_v<T, bool>) {
        value = (str == "true" || str == "1");
    } else {
        value = static_cast<T>(std::stoll(str));
    }
    return llvm::ConstantInt::get(Context, llvm::APInt(sizeof(T)*8, value, isSigned));
}

llvm::Value* CodeGenerator::EmitLiteralExpr(ASTNode* node) {
    if (node == nullptr || node->literal_expr.value.name == nullptr) {
        return nullptr;
    }

    const std::string literalStr = node->literal_expr.value.name;
    try {
        switch (node->inferred_type) {
            // 整数类型
            case TYPE_INT8:
                return CreateIntegerConstant<int8_t>(Context, literalStr, true);
            case TYPE_INT16:
                return CreateIntegerConstant<int16_t>(Context, literalStr, true);
            case TYPE_INT32:
                return CreateIntegerConstant<int32_t>(Context, literalStr, true);
            case TYPE_INT64:
                return CreateIntegerConstant<int64_t>(Context, literalStr, true);

            // 无符号整数
            case TYPE_UINT8:
                return CreateIntegerConstant<uint8_t>(Context, literalStr, false);
            case TYPE_UINT16:
                return CreateIntegerConstant<uint16_t>(Context, literalStr, false);
            case TYPE_UINT32:
                return CreateIntegerConstant<uint32_t>(Context, literalStr, false);
            case TYPE_UINT64:
                return CreateIntegerConstant<uint64_t>(Context, literalStr, false);

            // 浮点数
            case TYPE_FLOAT16: {
                float val = std::stof(literalStr);
                llvm::APFloat apVal(val);  // 先构造一个 float 类型的 APFloat
                bool losesInfo;
                apVal.convert(llvm::APFloat::IEEEhalf(), 
                            llvm::APFloat::rmNearestTiesToEven,
                            &losesInfo);
                return llvm::ConstantFP::get(Context, apVal);
            }
            case TYPE_FLOAT32: {
                float val = std::stof(literalStr);
                return llvm::ConstantFP::get(Context, llvm::APFloat(val));
            }
            case TYPE_FLOAT64: {
                double val = std::stod(literalStr);
                return llvm::ConstantFP::get(Context, llvm::APFloat(val));
            }

            // 字符串
            case TYPE_STRING: {
                llvm::Constant* strConst = llvm::ConstantDataArray::getString(Context, literalStr);
                llvm::GlobalVariable* GV = new llvm::GlobalVariable(
                    *Module,
                    strConst->getType(),
                    true,
                    llvm::GlobalValue::PrivateLinkage,
                    strConst,
                    "str.literal");

                if (Builder.GetInsertBlock()) {
                    return Builder.CreateInBoundsGEP(
                        GV->getValueType(),
                        GV,
                        {Builder.getInt32(0), Builder.getInt32(0)});
                }
                return GV;
            }

            // 字符
            case TYPE_CHAR:
                if (literalStr.size() >= 1) {
                    return llvm::ConstantInt::get(Context, llvm::APInt(8, literalStr[0], false));
                }
                return llvm::ConstantInt::get(Context, llvm::APInt(8, 0, false));

            // 布尔值
            case TYPE_BOOLEAN:
                return llvm::ConstantInt::get(Context, 
                    llvm::APInt(1, (literalStr == "true" || literalStr == "1"), false));

            // NULL（指针）
            case TYPE_PTR:
                return llvm::ConstantPointerNull::get(Builder.getPtrTy());

            default:
                return nullptr;
        }
    } catch (const std::exception& e) {
        // 处理转换错误
        return nullptr;
    }
}

/*
llvm::Value* CodeGenerator::EmitIdentifierExpr(ASTNode* expr) {
    llvm::Value* var = LookupSymbol((expr->identifier_expr).name);
    if (!var) throw std::runtime_error("Undefined variable: " + (expr->identifier_expr).name);
    return Builder.CreateLoad(ConvertToLLVMType(expr->inferred_type), var, (expr->identifier_expr).name);
}
*/

llvm::Value* CodeGenerator::EmitBinaryExpr(ASTNode* expr) {
    assert(expr->type == AST_BINARY_EXPR && "Expected binary expression node");
    const BinaryExpr& bin = expr->binary_expr;

    // 1. 生成左右子表达式的IR
    llvm::Value* L = EmitExpr(bin.left);
    llvm::Value* R = EmitExpr(bin.right);
    
    // 2. 获取统一的LLVM类型（处理类型提升）
    llvm::Type* type = ConvertToLLVMType(expr->inferred_type);

    // 3. 常量折叠优化（如果两边都是常量）
    if (llvm::isa<llvm::Constant>(L) && llvm::isa<llvm::Constant>(R)) {
        if (llvm::Constant* folded = ConstantFoldBinaryOp(bin.op, 
            llvm::cast<llvm::Constant>(L), 
            llvm::cast<llvm::Constant>(R),
            expr->inferred_type)) {
            return folded;
        }
    }

    // 4. 根据运算符类型生成对应IR
    switch (bin.op) {
        // 算术运算
        case OP_PLUS: 
            return Builder.CreateAdd(L, R, "addtmp");
        case OP_MINUS: 
            return Builder.CreateSub(L, R, "subtmp");
        case OP_STAR: 
            return Builder.CreateMul(L, R, "multmp");
        case OP_SLASH:
            if (type->isFPOrFPVectorTy()) {
                return Builder.CreateFDiv(L, R, "fdivtmp");
            } else {
                return Builder.CreateSDiv(L, R, "sdivtmp");
            }

        // 比较运算
        case OP_LT: case OP_LE: case OP_GT: case OP_GE: case OP_EQ: case OP_NE: {
            llvm::CmpInst::Predicate pred;
            if (type->isFPOrFPVectorTy()) {
                // 浮点比较
                switch (bin.op) {
                    case OP_LT: pred = llvm::CmpInst::FCMP_OLT; break;
                    case OP_LE: pred = llvm::CmpInst::FCMP_OLE; break;
                    case OP_GT: pred = llvm::CmpInst::FCMP_OGT; break;
                    case OP_GE: pred = llvm::CmpInst::FCMP_OGE; break;
                    case OP_EQ: pred = llvm::CmpInst::FCMP_OEQ; break;
                    case OP_NE: pred = llvm::CmpInst::FCMP_ONE; break;
                    default: llvm_unreachable("Invalid FP comparison");
                }
                return Builder.CreateFCmp(pred, L, R, "fcmptmp");
            } else {
                // 整数比较
                switch (bin.op) {
                    case OP_LT: pred = llvm::CmpInst::ICMP_SLT; break;
                    case OP_LE: pred = llvm::CmpInst::ICMP_SLE; break;
                    case OP_GT: pred = llvm::CmpInst::ICMP_SGT; break;
                    case OP_GE: pred = llvm::CmpInst::ICMP_SGE; break;
                    case OP_EQ: pred = llvm::CmpInst::ICMP_EQ;  break;
                    case OP_NE: pred = llvm::CmpInst::ICMP_NE;  break;
                    default: llvm_unreachable("Invalid integer comparison");
                }
                return Builder.CreateICmp(pred, L, R, "icmptmp");
            }
        }

        // 逻辑运算（布尔类型）
        case OP_AND: case OP_OR: {
            // 创建基本块实现短路求值
            llvm::Function* func = Builder.GetInsertBlock()->getParent();
            llvm::BasicBlock* lhsBlock = Builder.GetInsertBlock();
            llvm::BasicBlock* rhsBlock = llvm::BasicBlock::Create(Context, "bin.rhs", func);
            llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(Context, "bin.merge", func);

            // 根据运算符选择分支条件
            if (bin.op == OP_AND) {
                Builder.CreateCondBr(L, rhsBlock, mergeBlock);
            } else { // OP_OR
                Builder.CreateCondBr(L, mergeBlock, rhsBlock);
            }

            // 处理右操作数
            Builder.SetInsertPoint(rhsBlock);
            llvm::Value* rhsVal = EmitExpr(bin.right);
            Builder.CreateBr(mergeBlock);
            rhsBlock = Builder.GetInsertBlock(); // 可能被改变

            // 创建PHI节点合并结果
            Builder.SetInsertPoint(mergeBlock);
            llvm::PHINode* phi = Builder.CreatePHI(type, 2, "bintmp");
            if (bin.op == OP_AND) {
                phi->addIncoming(llvm::ConstantInt::get(type, 0), lhsBlock);
                phi->addIncoming(rhsVal, rhsBlock);
            } else {
                phi->addIncoming(llvm::ConstantInt::get(type, 1), lhsBlock);
                phi->addIncoming(rhsVal, rhsBlock);
            }
            return phi;
        }

        default:
            throw std::runtime_error("Unsupported binary operator at line " + 
                              std::to_string(expr->line));
    }
}
/*
llvm::Value* CodeGenerator::EmitAssignmentExpr(ASTNode* expr) {
    const BinaryExpr& assign = expr->assignment_expr;
    if (!assign.left || assign.left->type != AST_IDENTIFIER_EXPR)
        throw std::runtime_error("Invalid assignment target at line " + std::to_string(expr->line));

    llvm::Value* rhs = EmitExpr(assign.right);
    llvm::Value* var = LookupSymbol(assign.left->identifier_expr.name);
    if (!var) throw std::runtime_error("Undefined variable: " + assign.left->identifier_expr.name);
    
    Builder.CreateStore(rhs, var);
    return rhs;
}


llvm::Value* CodeGenerator::EmitCallExpr(ASTNode* expr) {
    const CallExpr& call = expr->call_expr;
    llvm::Function* callee = Module->getFunction(call.callee->);
    if (!callee)
        throw std::runtime_error("Unknown function: " + call.func_name + " at line " + std::to_string(expr->line));

    std::vector<llvm::Value*> args;
    for (const ASTNode* arg : call.args) {
        args.push_back(EmitExpr(arg));
        if (!args.back()) return nullptr;
    }

    if (callee->arg_size() != args.size())
        throw std::runtime_error("Argument count mismatch for " + call.func_name);

    return Builder.CreateCall(callee, args, "calltmp");
}

llvm::Value* CodeGenerator::EmitArrayAccessExpr(ASTNode* expr) {
    const ArrayAccessExpr& arr = expr->array_access_expr;
    llvm::Value* array = EmitExpr(arr.array);
    llvm::Value* index = EmitExpr(arr.index);
    
    if (EnableBoundsChecking) {
        emitBoundsCheck(Builder, array, index, expr->line);
    }

    return Builder.CreateInBoundsGEP(
        getLLVMType(expr->inferred_type), array, index, "arrayidx");
}

llvm::Value* CodeGenerator::EmitObjectAccessExpr(ASTNode* expr) {
    const ObjectAccessExpr& obj = expr->object_access_expr;
    llvm::Value* object = EmitExpr(obj.object);
    
    if (!object->getType()->isPointerTy() || 
        !object->getType()->getPointerElementType()->isStructTy()) {
        throw std::runtime_error("Invalid object access at line " + std::to_string(expr->line));
    }

    llvm::StructType* structTy = llvm::cast<llvm::StructType>(
        object->getType()->getPointerElementType());
    
    int memberIdx = getStructMemberIndex(structTy, obj.member_name);
    if (memberIdx == -1) {
        throw std::runtime_error("Unknown member '" + obj.member_name + 
                          "' at line " + std::to_string(expr->line));
    }

    return Builder.CreateStructGEP(structTy, object, memberIdx, obj.member_name + ".ptr");
}

llvm::Value* CodeGenerator::EmitAnonymousFuncExpr(ASTNode* expr) {
    const AnoymousFuncExpr& anon = expr->anonymous_func_expr;
    
    llvm::FunctionType* funcType = llvm::FunctionType::get(
        getLLVMType(anon.return_type), 
        getLLVMParamTypes(anon.parameters), 
        false);
    
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::PrivateLinkage, 
        "lambda", *Module);
    
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
    
    llvm::Value* bodyVal = EmitStmt(anon.body, func);
    
    if (!Builder.GetInsertBlock()->getTerminator()) {
        if (anon.return_type == VALUE_VOID) {
            Builder.CreateRetVoid();
        } else {
            Builder.CreateRet(bodyVal);
        }
    }
    
    return func;
}
*/
llvm::Value* CodeGenerator::EmitExpr(ASTNode* expr) {
    if (!expr) return nullptr;

    switch (expr->type) {
        case AST_LITERAL_EXPR:    return EmitLiteralExpr(expr);
        //case AST_IDENTIFIER_EXPR: return EmitIdentifierExpr(expr);
        case AST_BINARY_EXPR:     return EmitBinaryExpr(expr);
        //case AST_ASSIGNMENT_EXPR: return EmitAssignmentExpr(expr);
        //case AST_CALL_EXPR:       return EmitCallExpr(expr);
        //case AST_ARRAY_ACCESS_EXPR: return EmitArrayAccessExpr(expr);
        // AST_OBJECT_ACCESS_EXPR: return EmitObjectAccessExpr(expr);
        //case AST_ANONYMOUS_FUNC_EXPR: return EmitAnonymousFuncExpr(expr);
        default: throw std::runtime_error("Unsupported expression type at line " + std::to_string(expr->line));
    }
}

llvm::Value* CodeGenerator::EmitVarDecl(ASTNode *node) {
    // 1. 参数校验
    if (!node || node->type != AST_VAR_DECL) {
        throw std::runtime_error("Invalid variable declaration node");
    }

    VarDecl decl = node->var_decl;
    if (!decl.name.name) {
        throw std::runtime_error("Variable name is null at line " + 
                              std::to_string(node->line));
    }

    // 2. 类型处理
    llvm::Type* ty = ConvertToLLVMType(node->inferred_type);
    if (!ty) {
        throw std::runtime_error("Unsupported variable type at line " +
                              std::to_string(node->line));
    }

    // 3. 判断作用域（全局/局部）
    bool isGlobal = !Builder.GetInsertBlock();
    bool isConstant = (decl.var_type == VAR_TYPE_CONSTANT);
    std::string varName(decl.name.name ? decl.name.name : "name"); // 需保证ssa命名

    // 4. 处理全局变量
    if (isGlobal) {
        // 全局变量必须用常量初始化（或零初始化）
        llvm::Constant* initVal = llvm::Constant::getNullValue(ty);
        if (decl.value) {
            llvm::Value* v = EmitExpr(decl.value);
            initVal = llvm::dyn_cast<llvm::Constant>(v);
            if (!initVal) {
                throw std::runtime_error("Global variable initializer must be constant at line " +
                                      std::to_string(node->line));
            }
        }

        auto gv = new llvm::GlobalVariable(
            *Module,                    // LLVM模块
            ty,                        // 类型
            isConstant,                // 是否常量
            llvm::GlobalValue::ExternalLinkage, // 链接类型
            initVal,                   // 初始值
            varName                    // 变量名
        );
        
        // 设置对齐
        gv->setAlignment(llvm::Align(
            Module->getDataLayout().getABITypeAlign(ty).value()
        ));
        return gv;
    }
    // 5. 处理局部变量
    else {
        // 创建栈分配
        llvm::AllocaInst* alloc = Builder.CreateAlloca(
            ty,
            nullptr,                   // 数组大小
            varName                    // 变量名
        );
        
        // 设置对齐
        alloc->setAlignment(llvm::Align(
            Module->getDataLayout().getABITypeAlign(ty).value()
        ));

        // 处理初始化
        if (decl.value) {
            Builder.CreateStore(EmitExpr(decl.value), alloc);
        } else {
            Builder.CreateStore(llvm::Constant::getNullValue(ty), alloc);
        }

        return alloc;
    }
}

llvm::Function* CodeGenerator::EmitFunctionDecl(ASTNode *node) {
    if (!node) return nullptr;
    assert(node->type == AST_FUNC_DECL && "node->type != AST_FUNC_DECL");

    FunctionDecl funcDecl = node->func_decl;                                    
    // 1. 转换返回类型
    llvm::Type* retTy = ConvertToLLVMType(node->inferred_type);
    if (!retTy) {
        std::cerr << "Unsupported function return type: " << node->inferred_type << "\n";
        return nullptr;
    }

    // 2. 转换参数类型
    std::vector<llvm::Type*> paramTypes;
    for (Parameter* param = funcDecl.params; param != nullptr; param = param->next) {
        llvm::Type* ty = ConvertToLLVMType(param->inferred_type);
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
        funcType, llvm::Function::ExternalLinkage, funcDecl.name.name, *Module);

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
            EmitStmt(funcDecl.body, func);

            // 确保函数有终止指令
            if (!Builder.GetInsertBlock()->getTerminator()) {
                if (retTy->isVoidTy()) {
                    Builder.CreateRetVoid();
                } else {
                    throw std::runtime_error("Non-void function missing return statement");
                }
            }
        } catch (const std::exception& e) {
            // 发生错误时删除问题函数
            func->eraseFromParent();
            throw;
        }
    }

    return func;
}

llvm::Type* CodeGenerator::EmitStructOrUnionDecl(ASTNode *node) {
    if (!node) return nullptr;
    assert((node->type == AST_STRUCT_DECL || node->type == AST_UNION_DECL) && 
           "node->type != AST_STRUCT_OR_UNION_DECL");
    
    StructOrUnionDecl decl = node->struct_or_union_decl;
    bool isPacked = (node->type == AST_UNION_DECL);

    // 检查是否已存在该类型
    if (decl.name.name) {
        // 方法1：直接尝试创建，如果已存在会返回现有类型
        if (auto* existingType = llvm::StructType::getTypeByName(Context, decl.name.name)) {
            return existingType;
        }
    }

    // 创建新类型
    llvm::StructType* structType = llvm::StructType::create(Context, decl.name.name);

    // 收集成员类型
    std::vector<llvm::Type*> memberTypes;
    for (const MemberList* member = decl.members; member != nullptr; member = member->next) {
        ASTNode *member_decl = member->decl;
        if (!member_decl) continue;

        llvm::Type* memberType = nullptr;
        if (member_decl->type == AST_MEMBER_DECL) {
            memberType = ConvertToLLVMType(member_decl->inferred_type);
        } else if (member_decl->type == AST_STRUCT_DECL || member_decl->type == AST_UNION_DECL) {
            memberType = EmitStructOrUnionDecl(member_decl);
        }

        if (memberType) {
            memberTypes.push_back(memberType);
        }
    }

    // 设置结构体主体
    structType->setBody(memberTypes, isPacked);
    return structType;
}

llvm::Value* CodeGenerator::EmitDecl(ASTNode* decl, llvm::Function* currentFunction) {
    if (!decl) return nullptr;

    switch (decl->type) {
        case AST_VAR_DECL: {
            // 处理变量声明（全局/局部）
            return EmitVarDecl(decl);
        }
        case AST_FUNC_DECL: {
            // 处理函数声明/定义
            return EmitFunctionDecl(decl);
        }
        case AST_STRUCT_DECL: 
        case AST_UNION_DECL: {
            // 处理结构体/联合体定义
            EmitStructOrUnionDecl(decl);
        }
        default: {
            // 处理可执行语句（当currentFunction为nullptr时生成全局初始化代码）
            return EmitStmt(decl, currentFunction);
        }
    }
}

// 常量折叠优化
llvm::Constant* CodeGenerator::ConstantFoldBinaryOp(Operator op, llvm::Constant* L, llvm::Constant* R, TypeKind type) {
    if (type == TYPE_FLOAT32 || type == TYPE_FLOAT64) {
        auto* LF = llvm::cast<llvm::ConstantFP>(L);
        auto* RF = llvm::cast<llvm::ConstantFP>(R);
        llvm::APFloat lv = LF->getValueAPF();
        llvm::APFloat rv = RF->getValueAPF();
        
        switch (op) {
            case OP_PLUS: {
                lv.add(rv, llvm::APFloat::rmNearestTiesToEven);
                return llvm::ConstantFP::get(L->getContext(), lv);
            }
            case OP_MINUS: {
                lv.subtract(rv, llvm::APFloat::rmNearestTiesToEven);
                return llvm::ConstantFP::get(L->getContext(), lv);
            }
            case OP_STAR: {
                lv.multiply(rv, llvm::APFloat::rmNearestTiesToEven);
                return llvm::ConstantFP::get(L->getContext(), lv);
            }
            case OP_SLASH: {
                if (rv.isZero()) return nullptr;
                lv.divide(rv, llvm::APFloat::rmNearestTiesToEven);
                return llvm::ConstantFP::get(L->getContext(), lv);
            }
            case OP_LT:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) == llvm::APFloat::cmpLessThan));
            case OP_LE:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) != llvm::APFloat::cmpGreaterThan));
            case OP_GT:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) == llvm::APFloat::cmpGreaterThan));
            case OP_GE:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) != llvm::APFloat::cmpLessThan));
            case OP_EQ:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) == llvm::APFloat::cmpEqual));
            case OP_NE:  return llvm::ConstantInt::get(L->getContext(), 
                                  llvm::APInt(1, lv.compare(rv) != llvm::APFloat::cmpEqual));
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
    llvm::Value* len = Builder.CreateStructGEP(
        array->getType()->getNonOpaquePointerElementType(), 
        array, 
        0, 
        "len.ptr");
    
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

llvm::Type* CodeGenerator::ConvertStructType(const std::vector<llvm::Type*>& fieldTypes,
                                           const std::string& name,
                                           bool isPacked) {
    return llvm::StructType::create(Context, fieldTypes, name, isPacked);
}

llvm::FunctionType* CodeGenerator::ConvertFunctionType(llvm::Type* returnType,
                                      const std::vector<llvm::Type*>& paramTypes,
                                      bool isVarArg) {
    return llvm::FunctionType::get(returnType, paramTypes, isVarArg);
}

void CodeGenerator::EmitProgram(ASTNode *node) {
    if (!node) return;
    assert(node->type == AST_PROGRAM && "node->type != AST_PROGRAM");

    StmtSequence program = node->program;
    for (StatementList* stmtNode = program.statements; stmtNode != nullptr; stmtNode = stmtNode->next) {
        if (!stmtNode->statement) continue;
        EmitDecl(stmtNode->statement);
    }

    // 验证模块
    std::string verifyErrors;
    llvm::raw_string_ostream os(verifyErrors);
    if (llvm::verifyModule(*Module, &os)) {
        throw std::runtime_error("Module verification failed:\n" + os.str());
    }
    return;
}

void CodeGenerator::dumpIR() const {
    Module->print(llvm::outs(), nullptr);
}

//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o ir
/**
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
    llvm::Value* li8 = EmitLiteralExpr(TYPE_INT8, &i8);
    llvm::Value* i8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "i8.addr");
    Builder.CreateStore(li8, i8_alloca);

    int16_t i16 = -32768;
    llvm::Value* li16 = EmitLiteralExpr(TYPE_INT16, &i16);
    llvm::Value* i16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "i16.addr");
    Builder.CreateStore(li16, i16_alloca);

    int32_t i32 = 2147483647;
    llvm::Value* li32 = EmitLiteralExpr(TYPE_INT32, &i32);
    llvm::Value* i32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "i32.addr");
    Builder.CreateStore(li32, i32_alloca);

    int64_t i64 = -9223372036854775807LL;
    llvm::Value* li64 = EmitLiteralExpr(TYPE_INT64, &i64);
    llvm::Value* i64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "i64.addr");
    Builder.CreateStore(li64, i64_alloca);

    // 2. 无符号整型字面量
    uint8_t u8 = 255;
    llvm::Value* lu8 = EmitLiteralExpr(TYPE_UINT8, &u8);
    llvm::Value* u8_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "u8.addr");
    Builder.CreateStore(lu8, u8_alloca);

    uint16_t u16 = 65535;
    llvm::Value* lu16 = EmitLiteralExpr(TYPE_UINT16, &u16);
    llvm::Value* u16_alloca = Builder.CreateAlloca(Builder.getInt16Ty(), nullptr, "u16.addr");
    Builder.CreateStore(lu16, u16_alloca);

    uint32_t u32 = 4294967295;
    llvm::Value* lu32 = EmitLiteralExpr(TYPE_UINT32, &u32);
    llvm::Value* u32_alloca = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, "u32.addr");
    Builder.CreateStore(lu32, u32_alloca);

    uint64_t u64 = 18446744073709551615ULL;
    llvm::Value* lu64 = EmitLiteralExpr(TYPE_UINT64, &u64);
    llvm::Value* u64_alloca = Builder.CreateAlloca(Builder.getInt64Ty(), nullptr, "u64.addr");
    Builder.CreateStore(lu64, u64_alloca);

    // 3. 浮点数字面量
    uint16_t f16_bits = 0x3C00; // 1.0 in half precision
    llvm::Value* lf16 = EmitLiteralExpr(TYPE_FLOAT16, &f16_bits);
    llvm::Value* f16_alloca = Builder.CreateAlloca(Builder.getHalfTy(), nullptr, "f16.addr");
    Builder.CreateStore(lf16, f16_alloca);

    float f32 = 3.1415926f;
    llvm::Value* lf32 = EmitLiteralExpr(TYPE_FLOAT32, &f32);
    llvm::Value* f32_alloca = Builder.CreateAlloca(Builder.getFloatTy(), nullptr, "f32.addr");
    Builder.CreateStore(lf32, f32_alloca);

    double f64 = 2.718281828459045;
    llvm::Value* lf64 = EmitLiteralExpr(TYPE_FLOAT64, &f64);
    llvm::Value* f64_alloca = Builder.CreateAlloca(Builder.getDoubleTy(), nullptr, "f64.addr");
    Builder.CreateStore(lf64, f64_alloca);

    // 4. 字符串字面量
    const char* str = "Hello LLVM! 你好！";
    llvm::Value* lstr = EmitLiteralExpr(TYPE_STRING, str);
    llvm::Value* str_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "str.addr");
    Builder.CreateStore(lstr, str_alloca);

    // 5. 字符字面量
    char ch = 'A';
    llvm::Value* lch = EmitLiteralExpr(TYPE_CHAR, &ch);
    llvm::Value* ch_alloca = Builder.CreateAlloca(Builder.getInt8Ty(), nullptr, "ch.addr");
    Builder.CreateStore(lch, ch_alloca);

    // 6. 布尔字面量
    bool b = true;
    llvm::Value* lbool = EmitLiteralExpr(TYPE_BOOLEAN, &b);
    llvm::Value* bool_alloca = Builder.CreateAlloca(Builder.getInt1Ty(), nullptr, "bool.addr");
    Builder.CreateStore(lbool, bool_alloca);

    // 7. NULL字面量
    llvm::Value* lnull = EmitLiteralExpr(TYPE_PTR, nullptr);
    llvm::Value* null_alloca = Builder.CreateAlloca(Builder.getInt8PtrTy(), nullptr, "null.addr");
    Builder.CreateStore(lnull, null_alloca);

    // 函数返回
    Builder.CreateRetVoid();

    // 验证并打印模块
    if (llvm::verifyModule(Module, &llvm::errs())) {
    std::cerr << "Error verifying module!\n";
    return 1;
    }

    Module->print(llvm::outs(), nullptr);
    return 0;
}
 */