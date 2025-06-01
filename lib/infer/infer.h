#ifndef INFER_H
#define INFER_H

#include "../type/type.h"
#include "../ast/ast.h"

typedef struct ASTNode ASTNode; // 假设有AST节点定义

// 类型推断上下文
typedef struct {
    TypeEnv* env;       // 当前类型环境
    TypeEnv* global_env; // 全局类型环境
    bool has_errors;    // 是否有类型错误
} InferContext;

// 初始化推断上下文
InferContext* new_infer_context(TypeEnv* global_env);

// 释放推断上下文
void free_infer_context(InferContext* ctx);

// 类型推断入口
Type* infer_type(InferContext* ctx, ASTNode* node);

// 类型检查
bool check_type(InferContext* ctx, ASTNode* node, Type* expected);

#endif // INFER_H