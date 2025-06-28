#include "IR/DeclEmitter.h"
#include "IR/ExprEmitter.h"
#include <cassert>

llvm::Value* EmitVarDecl(llvm::IRBuilder<>& Builder, 
                        llvm::LLVMContext& Context,
                        llvm::Module* Module,
                        ASTNode *node) {
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

llvm::Function* EmitFunctionDecl(llvm::IRBuilder<>& Builder,
                                llvm::LLVMContext& Context,
                                llvm::Module* Module,
                                ASTNode *node) {
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

llvm::Type* EmitStructOrUnionDecl(llvm::LLVMContext& Context,
                                 llvm::Module* Module,
                                 ASTNode *node,
                                 bool isPacked = false) {
                                    
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

llvm::Value* EmitDecl(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     ASTNode* decl,
                     llvm::Function* currentFunction = nullptr) {
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
