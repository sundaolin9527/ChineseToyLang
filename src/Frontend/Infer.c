#include "Frontend/Infer.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define TYPE_COUNT (12)
// 类型提升规则说明：
// 1. 整数提升规则：
//    - 相同位数的有符号与无符号运算时，生成无符号类型，否则向更大范围的无符号类型提升
//    - 例如：INT8 + UINT16 -> INT32
// 2. 浮点提升规则：
//    - 任何浮点与整数运算都向浮点类型提升
//    - 浮点类型之间向更高精度提升
// 3. 特殊处理：
//    - 所有与UNKNOWN类型的运算结果都是UNKNOWN
// 类型提升规则表（[left][right] -> result）
static const TypeKind type_promotion_table[TYPE_COUNT][TYPE_COUNT] = {
    /* 左\右       UNK           I8             I16          I32           I64           U8            U16           U32           U64           F16           F32           F64 */
    /* UNKNOWN */ {TYPE_UNKNOWN, TYPE_UNKNOWN,TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN, TYPE_UNKNOWN},
    /* INT8    */ {TYPE_UNKNOWN, TYPE_INT8,   TYPE_INT16,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT8,   TYPE_UINT16,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64},
    /* INT16   */ {TYPE_UNKNOWN, TYPE_INT16,  TYPE_INT16,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT16,  TYPE_UINT16,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64},
    /* INT32   */ {TYPE_UNKNOWN, TYPE_INT32,  TYPE_INT32,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT32,  TYPE_UINT32,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT64},
    /* INT64   */ {TYPE_UNKNOWN, TYPE_INT64,  TYPE_INT64,   TYPE_INT64,   TYPE_INT64,   TYPE_UINT64,  TYPE_UINT64,  TYPE_UINT64,  TYPE_UINT64,  TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64},
    /* UINT8   */ {TYPE_UNKNOWN, TYPE_INT16,  TYPE_INT16,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT8,   TYPE_UINT16,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64},
    /* UINT16  */ {TYPE_UNKNOWN, TYPE_INT16,  TYPE_INT16,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT16,  TYPE_UINT16,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64},
    /* UINT32  */ {TYPE_UNKNOWN, TYPE_INT32,  TYPE_INT32,   TYPE_INT32,   TYPE_INT64,   TYPE_UINT32,  TYPE_UINT32,  TYPE_UINT32,  TYPE_UINT64,  TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT64},
    /* UINT64  */ {TYPE_UNKNOWN, TYPE_INT64,  TYPE_INT64,   TYPE_INT64,   TYPE_INT64,   TYPE_UINT64,  TYPE_UINT64,  TYPE_UINT64,  TYPE_UINT64,  TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64},
    /* FLOAT16 */ {TYPE_UNKNOWN, TYPE_FLOAT16,TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_FLOAT16, TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_FLOAT16, TYPE_FLOAT32, TYPE_FLOAT64},
    /* FLOAT32 */ {TYPE_UNKNOWN, TYPE_FLOAT32,TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT64, TYPE_FLOAT32, TYPE_FLOAT32, TYPE_FLOAT64},
    /* FLOAT64 */ {TYPE_UNKNOWN, TYPE_FLOAT64,TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64, TYPE_FLOAT64}
};

// 推断二元表达式类型
TypeKind infer_binary_expr_type(TypeEnv *env, ASTNode* node) {
    // 获取左右操作数类型
    TypeKind left_type = infer_type(env, node->binary_expr.left);
    TypeKind right_type = infer_type(env, node->binary_expr.right);
    Operator op = node->binary_expr.op;
    
    // 任一操作数为TYPE_ANY则直接返回
    if (left_type == TYPE_ANY || right_type == TYPE_ANY) {
        return TYPE_ANY;
    }

    // 处理比较和逻辑运算符
    if (op == OP_EQ || op == OP_NE || op == OP_LT || 
        op == OP_LE || op == OP_GT || op == OP_GE || 
        op == OP_AND || op == OP_OR) {
        return TYPE_BOOLEAN;
    }

    // 处理算术运算符
    if (op == OP_PLUS || op == OP_MINUS || op == OP_STAR || 
        op == OP_SLASH || op == OP_PERCENT || op == OP_POW) 
    {
        if (left_type > TYPE_COUNT || right_type > TYPE_COUNT)
        {
            return TYPE_UNKNOWN;
        }

        if (left_type == TYPE_CHAR) left_type = TYPE_UINT8;
        if (right_type == TYPE_CHAR) right_type = TYPE_UINT8;
        
        return type_promotion_table[left_type][right_type];
    }

    return TYPE_ANY;
}

// 推断字面量类型
TypeKind infer_literal_type(ASTNode* node) {
    switch (node->literal_expr.literal_type) {
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

// 推断标识符类型
TypeKind infer_identifier_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;

    Symbol* symbol = find_symbol_in_scope(env, node->identifier_expr.name);
    if (symbol != NULL) {
        return symbol->type;
    }
    return TYPE_ANY;
}

// 推断函数调用类型
TypeKind infer_call_expr_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;

    Symbol* symbol = find_symbol_in_scope(env, node->call_expr.callee->identifier_expr.name);
    if (symbol != NULL && symbol->type == TYPE_FUNCTION) {
        //return symbol->type->function.return_type;
    }
    return TYPE_ANY;
}

// 推断数组访问类型
TypeKind infer_array_access_type(TypeEnv *env, ASTNode* node) {
    return infer_type(env, node->array_access_expr.object);
}

// 获取对应的有符号类型
TypeKind get_signed_type(TypeKind unsigned_type) {
    switch(unsigned_type) {
        case TYPE_UINT8:  return TYPE_INT8;
        case TYPE_UINT16: return TYPE_INT16;
        case TYPE_UINT32: return TYPE_INT32;
        case TYPE_UINT64: return TYPE_INT64;
        default: return unsigned_type;
    }
}

TypeKind infer_unary_expr_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;

    TypeKind type = TYPE_ANY;
    TypeKind temp_type = TYPE_UNKNOWN;
    Operator op = node->unary_expr.op;

    switch (op)
    {
        case OP_NOT:
            type = TYPE_BOOLEAN;
            break;
        case OP_PLUS:
        case OP_MINUS:
            temp_type = infer_type(env, node->unary_expr.operand);
            type = get_signed_type(temp_type);
            break;
        case OP_STAR:
            temp_type = infer_type(env, node->unary_expr.operand);
            if (temp_type != TYPE_PTR) return TYPE_UNKNOWN;
            type = temp_type;
            break;
        default:
            break;
    }

    return type;
}

TypeKind infer_assignment_expr_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;

    TypeKind type = infer_type(env, node->assignment_expr.right);
    //修正标识符的类型
    if (node->assignment_expr.left){
        node->assignment_expr.left->inferred_type = type;
    }
    
    return type;
}

TypeKind infer_var_decl_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;

    if (node->var_decl.value) {
        return infer_type(env, node->var_decl.value);
    }
    return TYPE_ANY;
}

// 主类型推断函数
TypeKind infer_type(TypeEnv *env, ASTNode* node) {
    if (!node) return TYPE_ANY;
    
    if (node->inferred_type != TYPE_UNKNOWN) {
        return node->inferred_type;
    }
    
    switch (node->type) {
        case AST_UNARY_EXPR: return infer_unary_expr_type(env, node);
        case AST_ASSIGNMENT_EXPR: return infer_assignment_expr_type(env, node);
        case AST_OBJECT_ACCESS_EXPR:  break;
        case AST_LITERAL_EXPR: return infer_literal_type(node);
        case AST_IDENTIFIER_EXPR: return infer_identifier_type(env, node);
        case AST_BINARY_EXPR: return infer_binary_expr_type(env, node);
        case AST_CALL_EXPR: return infer_call_expr_type(env, node);
        case AST_ARRAY_ACCESS_EXPR: return infer_array_access_type(env, node);
        case AST_VAR_DECL: return infer_var_decl_type(env, node);
        case AST_FUNC_DECL: return TYPE_ANY;
        case AST_STRUCT_DECL: return TYPE_ANY;
        case AST_UNION_DECL: return TYPE_ANY;
        default: return TYPE_ANY;
    }
    return TYPE_ANY;
}
