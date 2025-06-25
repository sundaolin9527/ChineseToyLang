#include "IR/Emitter.h"

// 常量折叠优化
llvm::Constant* ConstantFoldBinaryOp(Operator op, llvm::Constant* L, llvm::Constant* R, TypeKind type) {
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
void emitBoundsCheck(llvm::IRBuilder<>& Builder, llvm::Value* array, llvm::Value* index, int line) {
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

llvm::Type* ConvertToLLVMType(llvm::LLVMContext& Context, 
                             llvm::IRBuilder<>& Builder,
                             TypeKind type) {
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

llvm::Type* ConvertArrayType(llvm::LLVMContext& Context,
                           llvm::Type* elementType,
                           uint64_t length) {
    if (!elementType) return nullptr;
    return llvm::ArrayType::get(elementType, length);
}

llvm::Type* ConvertStructType(llvm::LLVMContext& Context,
                             const std::vector<llvm::Type*>& fieldTypes,
                             const std::string& name = "",
                             bool isPacked = false) {
    return llvm::StructType::create(Context, fieldTypes, name, isPacked);
}

llvm::FunctionType* ConvertFunctionType(llvm::LLVMContext& Context,
                                      llvm::Type* returnType,
                                      const std::vector<llvm::Type*>& paramTypes,
                                      bool isVarArg = false) {
    return llvm::FunctionType::get(returnType, paramTypes, isVarArg);
}