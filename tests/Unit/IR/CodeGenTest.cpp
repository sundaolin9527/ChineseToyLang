#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Basic/Utils.h"
#include "IR/CodeGen.h"

int main() {
    llvm::LLVMContext Context;
    llvm::Module Module("literal_demo", Context);
    llvm::IRBuilder<> Builder(Context);

    // 创建演示函数
    llvm::FunctionType* funcType = llvm::FunctionType::get(Builder.getVoidTy(), false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "demo_literals", Module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(Context, "entry", func);
    Builder.SetInsertPoint(entry);

    // 函数返回
    Builder.CreateRetVoid();

    // 验证并打印模块
    if (llvm::verifyModule(Module, &llvm::errs())) {
        std::cerr << "Error verifying module!\n";
        return 1;
    }

    Module.print(llvm::outs(), nullptr);
    return 0;
}
