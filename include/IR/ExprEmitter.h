#ifndef EXPR_EMITTER_H
#define EXPR_EMITTER_H
#include "Emitter.h"

llvm::Value* EmitExpr(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     const ASTNode* expr);

#endif /* EXPR_EMITTER_H */