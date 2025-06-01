#include "infer.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

InferContext* new_infer_context(TypeEnv* global_env) {
    InferContext* ctx = malloc(sizeof(InferContext));
    ctx->env = NULL;
    ctx->global_env = global_env;
    ctx->has_errors = false;
    return ctx;
}

void free_infer_context(InferContext* ctx) {
    if (ctx) {
        free_type_env(ctx->env);
        free(ctx);
    }
}

Type* infer_type(InferContext* ctx, ASTNode* node) {
    if (!node || !ctx) return NULL;
    
    switch (node->type) {
        case AST_NUMBER_LITERAL:
            return strchr(node->value, '.') ? new_type(TYPE_FLOAT) : new_type(TYPE_INT);
            
        case AST_STRING_LITERAL:
            return new_type(TYPE_STRING);
            
        case AST_BOOL_LITERAL:
            return new_type(TYPE_BOOL);
            
        case AST_IDENTIFIER: {
            Type* t = lookup_type(ctx->env, node->value);
            if (!t) t = lookup_type(ctx->global_env, node->value);
            return t ? t : new_type(TYPE_UNKNOWN);
        }
            
        case AST_BINARY_EXPR: {
            Type* left = infer_type(ctx, node->children[0]);
            Type* right = infer_type(ctx, node->children[2]);
            Type* result = type_join(left, right);
            
            if (result->kind == TYPE_UNKNOWN) {
                fprintf(stderr, "类型错误: 无法推断操作符 %s 的类型\n", node->children[1]->value);
                ctx->has_errors = true;
            }
            
            free_type(left);
            free_type(right);
            return result;
        }
            
        case AST_CALL_EXPR: {
            Type* func_type = infer_type(ctx, node->children[0]);
            if (func_type->kind != TYPE_FUNCTION) {
                fprintf(stderr, "类型错误: %s 不是函数\n", node->children[0]->value);
                ctx->has_errors = true;
                free_type(func_type);
                return new_type(TYPE_UNKNOWN);
            }
            
            // 检查参数数量和类型
            if (node->children_count - 1 != func_type->meta.func.param_count) {
                fprintf(stderr, "类型错误: 函数 %s 需要 %d 参数，但提供了 %d\n",
                       node->children[0]->value,
                       func_type->meta.func.param_count,
                       node->children_count - 1);
                ctx->has_errors = true;
                free_type(func_type);
                return new_type(TYPE_UNKNOWN);
            }
            
            for (int i = 0; i < func_type->meta.func.param_count; i++) {
                Type* arg_type = infer_type(ctx, node->children[i+1]);
                if (!type_equal(arg_type, func_type->meta.func.param_types[i])) {
                    fprintf(stderr, "类型错误: 参数 %d 需要 %s 但提供了 %s\n",
                           i+1,
                           type_to_string(func_type->meta.func.param_types[i]),
                           type_to_string(arg_type));
                    ctx->has_errors = true;
                }
                free_type(arg_type);
            }
            
            Type* return_type = func_type->meta.func.return_type;
            free_type(func_type);
            return return_type;
        }
            
        // 其他AST节点类型的处理...
        default:
            return new_type(TYPE_UNKNOWN);
    }
}

bool check_type(InferContext* ctx, ASTNode* node, Type* expected) {
    Type* actual = infer_type(ctx, node);
    bool match = type_equal(actual, expected);
    
    if (!match) {
        fprintf(stderr, "类型错误: 需要 %s 但得到 %s\n",
               type_to_string(expected),
               type_to_string(actual));
        ctx->has_errors = true;
    }
    
    free_type(actual);
    return match;
}