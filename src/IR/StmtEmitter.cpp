#include "IR/StmtEmitter.h"

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

// 符号表作用域守卫
class SymbolTableScope {
    SymbolTable& table;
public:
    SymbolTableScope(SymbolTable& t) : table(t) { table.pushScope(); }
    ~SymbolTableScope() { table.popScope(); }
};

//=========================================

llvm::Value* EmitIfStmt(llvm::IRBuilder<>& Builder,
                       llvm::LLVMContext& Context,
                       llvm::Module* Module,
                       const IfStmt& ifStmt,
                       llvm::Function* currentFunction) {
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

llvm::Value* EmitForStmt(llvm::IRBuilder<>& Builder,
                        llvm::LLVMContext& Context,
                        llvm::Module* Module,
                        const ForStmt& forStmt,
                        llvm::Function* currentFunction) {
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

llvm::Value* EmitWhileStmt(llvm::IRBuilder<>& Builder,
                          llvm::LLVMContext& Context,
                          llvm::Module* Module,
                          const WhileStmt& whileStmt,
                          llvm::Function* currentFunction) {
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

llvm::Value* EmitExprStmt(llvm::IRBuilder<>& Builder,
                         llvm::LLVMContext& Context,
                         llvm::Module* Module,
                         const SimpleStmt& exprStmt,
                         llvm::Function* currentFunction) {
    if (exprStmt.expr) {
        // 生成表达式值，但忽略返回值（除非是块中最后一个表达式）
        return EmitExpr(Builder, Context, Module, exprStmt.expr);
    }
    return nullptr;
}

llvm::Value* EmitImportStmt(llvm::IRBuilder<>& Builder,
                           llvm::LLVMContext& Context,
                           llvm::Module* Module,
                           const Name& importStmt,
                           llvm::Function* currentFunction) {
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

llvm::Value* EmitExportStmt(llvm::IRBuilder<>& Builder,
                           llvm::LLVMContext& Context,
                           llvm::Module* Module,
                           const Name& exportStmt,
                           llvm::Function* currentFunction) {
    // 设置符号的链接属性
    if (llvm::GlobalValue* gv = Module->getNamedValue(exportStmt.name)) {
        gv->setLinkage(llvm::GlobalValue::ExternalLinkage);
        gv->setDSOLocal(true);
    }
    return nullptr;
}

llvm::Value* EmitReturnStmt(llvm::IRBuilder<>& Builder,
                           llvm::LLVMContext& Context,
                           llvm::Module* Module,
                           const SimpleStmt& returnStmt,
                           llvm::Function* currentFunction) {
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

llvm::Value* EmitBlockStmt(llvm::IRBuilder<>& Builder,
                          llvm::LLVMContext& Context,
                          llvm::Module* Module,
                          const StmtSequence& blockStmt,
                          llvm::Function* currentFunction) {
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

llvm::Value* EmitBreakStmt(llvm::IRBuilder<>& Builder,
                          llvm::LLVMContext& Context,
                          llvm::Module* Module,
                          llvm::Function* currentFunction) {
    if (auto* ctx = GetCurrentLoopContext()) {
        Builder.CreateBr(ctx->breakBB);
    }
    return nullptr;
}

llvm::Value* EmitContinueStmt(llvm::IRBuilder<>& Builder,
                             llvm::LLVMContext& Context,
                             llvm::Module* Module,
                             llvm::Function* currentFunction) {
    if (auto* ctx = GetCurrentLoopContext()) {
        Builder.CreateBr(ctx->continueBB);
    }
    return nullptr;
}

llvm::Value* EmitStmt(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     ASTNode* stmt,
                     llvm::Function* currentFunction) {
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