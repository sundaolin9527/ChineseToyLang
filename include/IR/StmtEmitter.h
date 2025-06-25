#ifndef STMT_EMITTER_H
#define STMT_EMITTER_H
#include "Emitter.h"

llvm::Value* EmitStmt(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     const ASTNode* stmt,
                     llvm::Function* currentFunction);

#endif /* STMT_EMITTER_H */