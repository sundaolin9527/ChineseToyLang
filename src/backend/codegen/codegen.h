#ifndef CODEGEN_H
#define CODEGEN_H

#include "../ast/ast.h"
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

typedef struct {
    LLVMModuleRef module;
    LLVMBuilderRef builder;
    LLVMValueRef current_function;
} CodegenContext;

// 初始化代码生成上下文
CodegenContext* init_codegen_context(const char* module_name);

// 生成LLVM IR
void generate_ir(CodegenContext* context, ASTNode* ast);

// 释放代码生成上下文
void free_codegen_context(CodegenContext* context);

// 打印生成的IR
void print_ir(CodegenContext* context);

#endif // CODEGEN_H