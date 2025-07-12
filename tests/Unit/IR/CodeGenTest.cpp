#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <memory>
#include "Frontend/Lexer.h"
#include "Frontend/Parser.h"
#include "Basic/Utils.h"
#include "IR/CodeGen.h"

using namespace llvm;
int main(int argc, char *argv[]) {
    // 设置本地化环境以支持中文
    /** 
    setlocale(LC_ALL, "");
    
    // 检查命令行参数
    if (argc != 2) {
        fprintf(stderr, "用法: %s <源代码文件>\n", argv[0]);
        return 1;
    }
    
    char* source = read_file(argv[1]);
    if (!source) {
        return EXIT_FAILURE;
    }

    printf("正在分析文件: %s\n", argv[1]);
    printf("========================================\n");
    
    printf("从文件 '%s' 读取源代码...\n", argv[1]);
    printf("%s", source);
    // 创建词法分析器
    Lexer *lexer = init_lexer(source);
    
    // 创建语法分析器
    Parser *parser = init_parser(lexer);
    
    // 解析程序
    ASTNode *program = parse_program(parser);
    
    // 打印AST
    printf("\n生成的抽象语法树:\n");
    print_ast(program, 0);

    CodeGenerator codeGenerator("emitPragram");
    codeGenerator.EmitProgram(program);
    codeGenerator.dumpIR();

    // 释放内存
    free_ast_node(program);
    free(lexer);
    free_parser(&parser);
    free(source);
    */

    // 初始化 LLVM 环境
    LLVMContext context;
    std::unique_ptr<Module> module = std::make_unique<Module>("test_module", context);
    IRBuilder<> builder(context);

    // 1. 全局变量
    GlobalVariable *globalVar = new GlobalVariable(
        *module, 
        Type::getInt32Ty(context), 
        false,
        GlobalValue::CommonLinkage,
        ConstantInt::get(Type::getInt32Ty(context), 0),
        "global_var"
    );
    globalVar->setAlignment(MaybeAlign(4));

    // 2. 全局常量
    GlobalVariable *globalConst = new GlobalVariable(
        *module,
        Type::getInt32Ty(context),
        true,
        GlobalValue::InternalLinkage,
        ConstantInt::get(Type::getInt32Ty(context), 100),
        "global_const"
    );
    globalConst->setAlignment(MaybeAlign(4));

    // 3. test 函数
    FunctionType *funcType = FunctionType::get(
        Type::getInt32Ty(context),
        false
    );
    Function *testFunc = Function::Create(
        funcType,
        Function::ExternalLinkage,
        "test",
        module.get()
    );

    // test 函数体
    BasicBlock *entryBB = BasicBlock::Create(context, "entry", testFunc);
    builder.SetInsertPoint(entryBB);

    // 4. 局部变量
    Value *localVar = builder.CreateAlloca(Type::getInt32Ty(context), nullptr, "local_var");
    cast<AllocaInst>(localVar)->setAlignment(Align(4));
    builder.CreateStore(ConstantInt::get(Type::getInt32Ty(context), 0), localVar);

    // 5. 使用常量
    Value *constVal = builder.CreateAdd(
        ConstantInt::get(Type::getInt32Ty(context), 10),
        ConstantInt::get(Type::getInt32Ty(context), 20),
        "const_val"
    );

    // 6. 使用全局常量
    Value *loadedGlobalConst = builder.CreateLoad(Type::getInt32Ty(context), globalConst, "loaded_global_const");
    Value *sum1 = builder.CreateAdd(constVal, loadedGlobalConst, "sum1");

    // 7. 修改全局变量
    builder.CreateStore(sum1, globalVar);

    // 8. 修改局部变量
    Value *localVal = builder.CreateLoad(Type::getInt32Ty(context), localVar, "local_val");
    Value *newLocalVal = builder.CreateAdd(localVal, sum1, "new_local_val");
    builder.CreateStore(newLocalVal, localVar);

    // 9. 返回值
    Value *result = builder.CreateLoad(Type::getInt32Ty(context), localVar, "result");
    builder.CreateRet(result);

    // 10. main 函数
    Function *mainFunc = Function::Create(
        funcType,
        Function::ExternalLinkage,
        "main",
        module.get()
    );

    BasicBlock *mainEntryBB = BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(mainEntryBB);

    Value *testResult = builder.CreateCall(testFunc, {}, "test_result");
    Value *globalVal = builder.CreateLoad(Type::getInt32Ty(context), globalVar, "global_val");
    Value *check = builder.CreateICmpEQ(testResult, globalVal, "check");

    BasicBlock *successBB = BasicBlock::Create(context, "success", mainFunc);
    BasicBlock *failBB = BasicBlock::Create(context, "fail", mainFunc);
    builder.CreateCondBr(check, successBB, failBB);

    builder.SetInsertPoint(successBB);
    builder.CreateRet(ConstantInt::get(Type::getInt32Ty(context), 0));

    builder.SetInsertPoint(failBB);
    builder.CreateRet(ConstantInt::get(Type::getInt32Ty(context), 1));

    // 验证
    verifyFunction(*testFunc);
    verifyFunction(*mainFunc);
    verifyModule(*module);

    module->print(outs(), nullptr);
    return 0;
    /**
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
     */
}
