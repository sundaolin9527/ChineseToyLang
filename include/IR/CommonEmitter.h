#ifndef EMITTER_H
#define EMITTER_H

#include <iostream>
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Verifier.h"
#include "Frontend/Ast.h"
#include "Basic/Types.h"


llvm::Constant* ConstantFoldBinaryOp(Operator op, llvm::Constant* L, llvm::Constant* R, TypeKind type);
void emitBoundsCheck(llvm::IRBuilder<>& Builder, llvm::Value* array, llvm::Value* index, int line);
llvm::Type* ConvertToLLVMType(llvm::LLVMContext& Context, llvm::IRBuilder<>& Builder, TypeKind type);
llvm::Type* ConvertArrayType(llvm::LLVMContext& Context, llvm::Type* elementType, uint64_t length);
llvm::Type* ConvertStructType(llvm::LLVMContext& Context, const std::vector<llvm::Type*>& fieldTypes,
                             const std::string& name = "", bool isPacked = false);
llvm::FunctionType* ConvertFunctionType(llvm::LLVMContext& Context, llvm::Type* returnType,
                                      const std::vector<llvm::Type*>& paramTypes, bool isVarArg = false);
#endif /* EMITTER_H */