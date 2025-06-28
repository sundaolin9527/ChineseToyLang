#ifndef DECL_EMITTER_H
#define DECL_EMITTER_H
#include "CommonEmitter.h"

llvm::Value* EmitDecl(llvm::IRBuilder<>& Builder,
                     llvm::LLVMContext& Context,
                     llvm::Module* Module,
                     ASTNode* decl,
                     llvm::Function* currentFunction = nullptr);

#endif /* DECL_EMITTER_H */