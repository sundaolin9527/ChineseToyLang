#include "infer.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

// 推断字面量类型
Type infer_literal_type(ASTNode* node) {
    switch (node->literal.literal_type) {
        case LITERAL_INT8:    return TYPE_INT8;
        case LITERAL_INT16:   return TYPE_INT16;
        case LITERAL_INT32:   return TYPE_INT32;
        case LITERAL_INT64:   return TYPE_INT64;
        case LITERAL_UINT8:   return TYPE_UINT8;
        case LITERAL_UINT16:  return TYPE_UINT16;
        case LITERAL_UINT32:  return TYPE_UINT32;
        case LITERAL_UINT64:  return TYPE_UINT64;
        case LITERAL_FLOAT16: return TYPE_FLOAT16;
        case LITERAL_FLOAT32: return TYPE_FLOAT32;
        case LITERAL_FLOAT64: return TYPE_FLOAT64;
        case LITERAL_STRING:  return TYPE_STRING;
        case LITERAL_CHAR:    return TYPE_CHAR;
        case LITERAL_BOOLEAN: return TYPE_BOOLEAN;
        case LITERAL_NULL:    return TYPE_PTR;
        default: return TYPE_ANY;
    }
}

// 推断二元表达式类型
Type infer_binary_expr_type(TypeEnv *env, ASTNode* node) {
    Type left_type = infer_type(env, node->binary_expr.left);
    Type right_type = infer_type(env, node->binary_expr.right);
    Operator op = node->binary_expr.operator;

    // 算术运算符
    if (op == OP_PLUS || op == OP_MINUS || op == OP_STAR || 
        op == OP_SLASH || op == OP_PERCENT)
    {
        switch (left_type)
        {
            case TYPE_INT8:
                if(right_type == TYPE_CHAR){
                    return TYPE_INT8;
                }
                else if (right_type == TYPE_INT8 || right_type == TYPE_INT16 ||
                        right_type == TYPE_INT32 || right_type == TYPE_INT64 ||
                        right_type == TYPE_FLOAT16 || right_type == TYPE_FLOAT32 ||
                        right_type == TYPE_FLOAT64)
                {
                    return right_type;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_INT16:
                if (right_type == TYPE_INT8 || right_type == TYPE_CHAR){
                    return TYPE_INT16;
                }
                else if (right_type == TYPE_INT16 || right_type == TYPE_INT32 || 
                        right_type == TYPE_INT64 || right_type == TYPE_FLOAT16 || 
                        right_type == TYPE_FLOAT32 || right_type == TYPE_FLOAT64)
                {
                    return right_type;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_INT32:
                if (right_type == TYPE_INT8 || right_type == TYPE_INT16 || right_type == TYPE_CHAR){
                    return TYPE_INT32;
                }
                else if (right_type == TYPE_INT32 ||  right_type == TYPE_INT64 || 
                        right_type == TYPE_FLOAT16 || right_type == TYPE_FLOAT32 ||
                        right_type == TYPE_FLOAT64)
                {
                    return right_type;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_INT64:
                if (right_type == TYPE_INT8 || right_type == TYPE_INT16 ||
                    right_type == TYPE_INT32 || right_type == TYPE_INT64 ||
                    right_type == TYPE_FLOAT16 || right_type == TYPE_FLOAT32 ||
                    right_type == TYPE_FLOAT64 || right_type == TYPE_CHAR)
                {
                    return TYPE_INT64;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_FLOAT16:
                if (right_type == TYPE_INT8 || right_type == TYPE_INT16 || right_type == TYPE_FLOAT16 ||
                    right_type == TYPE_CHAR)
                {
                    return TYPE_FLOAT16;
                }
                else if (right_type == TYPE_INT32 || right_type == TYPE_FLOAT32)
                {
                    return TYPE_FLOAT32;
                }
                else if (right_type == TYPE_INT64 || right_type == TYPE_FLOAT64)
                {
                    return TYPE_FLOAT64;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_FLOAT32:
                if (right_type == TYPE_INT8 || right_type == TYPE_INT16 || right_type == TYPE_FLOAT16 ||
                    right_type == TYPE_INT32 || right_type == TYPE_FLOAT32 || right_type == TYPE_CHAR)
                {
                    return TYPE_FLOAT32;
                }
                else if (right_type == TYPE_INT64 || right_type == TYPE_FLOAT64)
                {
                    return TYPE_FLOAT64;
                }
                else
                {
                    return TYPE_ERROR;
                }
            case TYPE_FLOAT64:
                if (right_type == TYPE_INT8 || right_type == TYPE_INT16 || right_type == TYPE_FLOAT16 ||
                    right_type == TYPE_INT32 || right_type == TYPE_FLOAT32 ||right_type == TYPE_INT64 || 
                    right_type == TYPE_FLOAT64 || right_type == TYPE_CHAR)
                {
                    return TYPE_FLOAT64;
                }
                else
                {
                    return TYPE_ERROR;
                }
            //指针类型先不考虑
            default:
                break;
        }
    }
    
    if (op == OP_EQ || op == OP_NE || op == OP_LT || 
        op == OP_LE || op == OP_GT || op == OP_GE) 
    {
        return TYPE_BOOLEAN;
    }
    
    // 逻辑运算符
    if (op == OP_AND || op == OP_OR) {
        
        return TYPE_BOOLEAN;
    }
    
    // 指数运算
    if (op == OP_POW)
    {
        return TYPE_FLOAT64;
    }
    
    return TYPE_ANY;
}

// 推断标识符类型
Type infer_identifier_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_VOID;

    Symbol* symbol = find_symbol_in_scope(env, node->identifier.name);
    if (symbol != NULL) {
        return symbol->type;
    }
    return TYPE_ANY;
}

// 推断函数调用类型
Type infer_call_expr_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_VOID;

    Symbol* symbol = find_symbol_in_scope(env, node->call.callee->identifier.name);
    if (symbol != NULL && symbol->type == TYPE_FUNCTION) {
        //return symbol->type->function.return_type;
    }
    return TYPE_ANY;
}

// 推断数组访问类型
Type infer_array_access_type(TypeEnv *env, ASTNode* node) {
    return infer_type(env, node->array_access.object);
}

// 主类型推断函数
Type infer_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_VOID;
    
    if (node->inferred_type != TYPE_UNKNOWN) {
        return node->inferred_type;
    }
    
    Type type = TYPE_UNKNOWN;
    switch (node->type) {
        case AST_UNARY_EXPR:
           break;
        case  AST_ASSIGNMENT_EXPR:
            break;
        case AST_OBJECT_ACCESS_EXPR:
            break;
        case AST_LITERAL_EXPR:
            type = infer_literal_type(node);
            break;
        case AST_IDENTIFIER_EXPR:
            type = infer_identifier_type(env, node);
            break;
        case AST_BINARY_EXPR:
            type = infer_binary_expr_type(env, node);
            break;
        case AST_CALL_EXPR:
            type = infer_call_expr_type(env, node);
            break;
        case AST_ARRAY_ACCESS_EXPR:
            type = infer_array_access_type(env, node);
            break;
        case AST_VAR_DECL:
            if (node->var_decl.value != NULL) {
                type = infer_type(env, node->var_decl.value);
            } else {
                type = TYPE_ANY;
            }
            break;
        case AST_FUNC_DECL:
        case AST_STRUCT_DECL:
        case AST_UNION_DECL:
            type = TYPE_ANY;
            break;
        default:
            type = TYPE_ANY;
            break;
    }
    
    node->inferred_type = type;
    return type;
}
