#include "IR/ExprEmitter.h"

llvm::Value* EmitLiteral(llvm::IRBuilder<> &Builder, llvm::LLVMContext &Context, llvm::Module *Module, TypeKind type, const void *value) 
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

    // 字符串（全局常量）
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

llvm::Value* EmitLiteralExpr(llvm::IRBuilder<>& Builder,
                           llvm::LLVMContext& Context,
                           llvm::Module* Module,
                           const ASTNode* expr) {
    const LiteralExpr& lit = expr->literal_expr;
    switch (expr->inferred_type) {
        case VALUE_INT:
            return llvm::ConstantInt::get(Context, llvm::APInt(32, lit.value.int_val, true));
        case VALUE_FLOAT:
            return llvm::ConstantFP::get(Context, llvm::APFloat(lit.value.float_val));
        case VALUE_BOOL:
            return llvm::ConstantInt::get(Context, llvm::APInt(1, lit.value.bool_val));
        case VALUE_STRING: {
            llvm::Constant* strConst = llvm::ConstantDataArray::getString(Context, lit.value.str_val);
            llvm::GlobalVariable* gv = new llvm::GlobalVariable(
                *Module, strConst->getType(), true,
                llvm::GlobalValue::PrivateLinkage, strConst, ".str");
            return Builder.CreateInBoundsGEP(gv, {
                Builder.getInt32(0), Builder.getInt32(0) });
        }
        default:
            throw CodegenError("Unsupported literal type at line " + std::to_string(expr->line));
    }
}

llvm::Value* EmitIdentifierExpr(llvm::IRBuilder<>& Builder,
                              llvm::LLVMContext& Context,
                              llvm::Module* Module,
                              const ASTNode* expr) {
    llvm::Value* var = LookupSymbol(expr->identifier_expr.name);
    if (!var) throw CodegenError("Undefined variable: " + expr->identifier_expr.name);
    return Builder.CreateLoad(getLLVMType(expr->inferred_type), var, expr->identifier_expr.name.c_str());
}

llvm::Value* EmitBinaryExpr(llvm::IRBuilder<>& Builder,
                          llvm::LLVMContext& Context,
                          llvm::Module* Module,
                          const ASTNode* expr) {
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

llvm::Value* EmitAssignmentExpr(llvm::IRBuilder<>& Builder,
                               llvm::LLVMContext& Context,
                               llvm::Module* Module,
                               const ASTNode* expr) {
    const BinaryExpr& assign = expr->assignment_expr;
    if (!assign.left || assign.left->type != AST_IDENTIFIER_EXPR)
        throw CodegenError("Invalid assignment target at line " + std::to_string(expr->line));

    llvm::Value* rhs = EmitExpr(Builder, Context, Module, assign.right);
    llvm::Value* var = LookupSymbol(assign.left->identifier_expr.name);
    if (!var) throw CodegenError("Undefined variable: " + assign.left->identifier_expr.name);
    
    Builder.CreateStore(rhs, var);
    return rhs;
}

llvm::Value* EmitCallExpr(llvm::IRBuilder<>& Builder,
                         llvm::LLVMContext& Context,
                         llvm::Module* Module,
                         const ASTNode* expr) {
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

llvm::Value* EmitArrayAccessExpr(llvm::IRBuilder<>& Builder,
                                llvm::LLVMContext& Context,
                                llvm::Module* Module,
                                const ASTNode* expr) {
    const ArrayAccessExpr& arr = expr->array_access_expr;
    llvm::Value* array = EmitExpr(Builder, Context, Module, arr.array);
    llvm::Value* index = EmitExpr(Builder, Context, Module, arr.index);
    
    if (EnableBoundsChecking) {
        emitBoundsCheck(Builder, array, index, expr->line);
    }

    return Builder.CreateInBoundsGEP(
        getLLVMType(expr->inferred_type), array, index, "arrayidx");
}

llvm::Value* EmitObjectAccessExpr(llvm::IRBuilder<>& Builder,
                                 llvm::LLVMContext& Context,
                                 llvm::Module* Module,
                                 const ASTNode* expr) {
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

llvm::Value* EmitAnonymousFuncExpr(llvm::IRBuilder<>& Builder,
                                  llvm::LLVMContext& Context,
                                  llvm::Module* Module,
                                  const ASTNode* expr) {
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

llvm::Value* EmitExpr(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     const ASTNode* expr) {
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