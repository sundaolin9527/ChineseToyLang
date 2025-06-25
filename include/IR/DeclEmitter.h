#ifndef DECL_EMITTER_H
#define DECL_EMITTER_H
#include "Emitter.h"

llvm::Value* EmitVarDecl(llvm::IRBuilder<>& Builder, 
                        llvm::LLVMContext& Context,
                        llvm::Module* Module,
                        const VarDecl& decl);

llvm::Function* EmitFunctionDecl(llvm::IRBuilder<>& Builder,
                                llvm::LLVMContext& Context,
                                llvm::Module* Module,
                                const FunctionDecl& funcDecl);

llvm::Type* EmitStructOrUnionDecl(llvm::LLVMContext& Context,
                                 llvm::Module* Module,
                                 const StructOrUnionDecl& decl,
                                 bool isPacked = false);
#endif /* DECL_EMITTER_H */