#include "codegen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static LLVMValueRef codegen_expr(CodegenContext* context, ASTNode* node);
static LLVMValueRef codegen_function(CodegenContext* context, ASTNode* node);
static LLVMValueRef codegen_binary_expr(CodegenContext* context, ASTNode* node);
static LLVMValueRef codegen_if_stmt(CodegenContext* context, ASTNode* node);

CodegenContext* init_codegen_context(const char* module_name) {
    CodegenContext* context = malloc(sizeof(CodegenContext));
    context->module = LLVMModuleCreateWithName(module_name);
    context->builder = LLVMCreateBuilder();
    context->current_function = NULL;
    return context;
}

void free_codegen_context(CodegenContext* context) {
    if (context) {
        LLVMDisposeBuilder(context->builder);
        LLVMDisposeModule(context->module);
        free(context);
    }
}

void generate_ir(CodegenContext* context, ASTNode* ast) {
    if (!ast || !context) return;
    
    // 为每个函数生成IR
    for (int i = 0; i < ast->children_count; i++) {
        ASTNode* child = ast->children[i];
        if (child->type == AST_FUNCTION_DECL) {
            codegen_function(context, child);
        }
    }
}

static LLVMValueRef codegen_function(CodegenContext* context, ASTNode* node) {
    // 1. 准备函数参数类型
    LLVMTypeRef param_types[2]; // 假设最多2个参数
    int param_count = 0;
    
    // 计算参数数量 (跳过函数名节点)
    for (int i = 1; i < node->children_count; i++) {
        if (node->children[i]->type == AST_IDENTIFIER) {
            param_types[param_count++] = LLVMInt32Type(); // 假设所有参数都是i32
        }
    }
    
    // 2. 创建函数类型 (返回i32，参数i32)
    LLVMTypeRef ret_type = LLVMInt32Type();
    LLVMTypeRef func_type = LLVMFunctionType(ret_type, param_types, param_count, 0);
    
    // 3. 创建函数
    LLVMValueRef func = LLVMAddFunction(
        context->module, 
        node->children[0]->value, // 函数名
        func_type
    );
    
    context->current_function = func;
    
    // 4. 创建基本块
    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");
    LLVMPositionBuilderAtEnd(context->builder, entry);
    
    // 5. 生成函数体
    ASTNode* block = node->children[node->children_count - 1]; // 最后一个子节点是block
    for (int i = 0; i < block->children_count; i++) {
        ASTNode* stmt = block->children[i];
        if (stmt->type == AST_RETURN_STATEMENT) {
            LLVMValueRef ret_val = codegen_expr(context, stmt->children[0]);
            LLVMBuildRet(context->builder, ret_val);
        }
    }
    
    return func;
}

static LLVMValueRef codegen_expr(CodegenContext* context, ASTNode* node) {
    if (!node) return NULL;
    
    switch (node->type) {
        case AST_BINARY_EXPR:
            return codegen_binary_expr(context, node);
        case AST_IDENTIFIER: {
            // 查找参数 (简化处理，实际应该使用符号表)
            LLVMValueRef func = context->current_function;
            const char* name = node->value;
            
            for (unsigned i = 0; i < LLVMCountParams(func); i++) {
                LLVMValueRef param = LLVMGetParam(func, i);
                if (strcmp(LLVMGetValueName(param), name) == 0) {
                    return param;
                }
            }
            return NULL;
        }
        case AST_NUMBER_LITERAL: {
            int num = atoi(node->value);
            return LLVMConstInt(LLVMInt32Type(), num, 0);
        }
        default:
            return NULL;
    }
}

static LLVMValueRef codegen_binary_expr(CodegenContext* context, ASTNode* node) {
    LLVMValueRef L = codegen_expr(context, node->children[0]);
    LLVMValueRef R = codegen_expr(context, node->children[2]);
    
    if (!L || !R) return NULL;
    
    const char* op = node->children[1]->value;
    
    if (strcmp(op, "+") == 0) {
        return LLVMBuildAdd(context->builder, L, R, "addtmp");
    } else if (strcmp(op, "-") == 0) {
        return LLVMBuildSub(context->builder, L, R, "subtmp");
    } else if (strcmp(op, "*") == 0) {
        return LLVMBuildMul(context->builder, L, R, "multmp");
    } else if (strcmp(op, "/") == 0) {
        return LLVMBuildSDiv(context->builder, L, R, "divtmp");
    }
    
    return NULL;
}

void print_ir(CodegenContext* context) {
    char* ir = LLVMPrintModuleToString(context->module);
    printf("%s\n", ir);
    LLVMDisposeMessage(ir);
}