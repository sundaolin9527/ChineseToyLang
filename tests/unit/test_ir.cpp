#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

int main() {
    // 1. 创建核心对象
    LLVMContext context;
    IRBuilder<> builder(context);
    Module module("my_module", context);

    // 2. 创建函数原型: int add(int a, int b)
    Type* int32 = Type::getInt32Ty(context);
    FunctionType* funcType = FunctionType::get(int32, {int32, int32}, false);
    Function* addFunc = Function::Create(
        funcType, Function::ExternalLinkage, "add", module
    );

    // 3. 创建基本块和IR指令
    BasicBlock* entry = BasicBlock::Create(context, "entry", addFunc);
    builder.SetInsertPoint(entry);

    Value* a = addFunc->getArg(0);
    Value* b = addFunc->getArg(1);
    Value* result = builder.CreateAdd(a, b, "sum");
    builder.CreateRet(result);

    // 4. 打印IR到标准输出
    module.print(outs(), nullptr);

    return 0;
}