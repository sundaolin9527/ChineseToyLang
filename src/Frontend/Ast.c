#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "Frontend/Ast.h"

#define AST_ERROR_EXIT(fmt, ...) \
    do { \
        fprintf(stderr, "[AST错误] " fmt " (%s:%d)\n", ##__VA_ARGS__, __FILE__, __LINE__); \
        exit(EXIT_FAILURE); \
    } while (0)

void print_ast(ASTNode *node, int depth);

Operator token_to_operator(TokenType token) {
    if (token == TOKEN_PLUS) return OP_PLUS;
    if (token == TOKEN_MINUS) return OP_MINUS;
    if (token == TOKEN_STAR) return OP_STAR;
    if (token == TOKEN_SLASH) return OP_SLASH;
    if (token == TOKEN_PERCENT) return OP_PERCENT;
    if (token == TOKEN_POW) return OP_POW;
    if (token == TOKEN_EQ) return OP_EQ;
    if (token == TOKEN_NE) return OP_NE;
    if (token == TOKEN_GT) return OP_GT;
    if (token == TOKEN_LT) return OP_LT;
    if (token == TOKEN_GE) return OP_GE;
    if (token == TOKEN_LE) return OP_LE;
    if (token == TOKEN_AND) return OP_AND;
    if (token == TOKEN_OR) return OP_OR;
    if (token == TOKEN_NOT) return OP_NOT;
    if (token == TOKEN_ASSIGN) return OP_ASSIGN;
    if (token == TOKEN_ADD_ASSIGN) return OP_ADD_ASSIGN;
    if (token == TOKEN_SUB_ASSIGN) return OP_SUB_ASSIGN;
    if (token == TOKEN_MUL_ASSIGN) return OP_MUL_ASSIGN;
    if (token == TOKEN_DIV_ASSIGN) return OP_DIV_ASSIGN;
    if (token == TOKEN_MOD_ASSIGN) return OP_MOD_ASSIGN;
    return OP_UNKNOWN;
}

/* 创建新的AST节点 */
ASTNode* new_ast_node(ASTNodeType type, int line) {
    ASTNode *node = (ASTNode*)malloc(sizeof(ASTNode));
    if (!node) {
        AST_ERROR_EXIT("内存分配失败\n");
    }
    
    node->type = type;
    node->line = line;
    node->inferred_type = TYPE_UNKNOWN;

    /* 初始化联合体 */
    memset(&node->program, 0, sizeof(node->program));
    
    return node;
}

/* 创建新的参数节点 */
Parameter* new_parameter(const char *name, ASTNode *default_value) {
    Parameter *param = (Parameter*)malloc(sizeof(Parameter));
    if (!param) {
        AST_ERROR_EXIT("内存分配失败\n");
    }
    
    param->name.name = strdup(name);
    param->default_value = default_value;
    param->next = NULL;
    
    return param;
}

/* 创建新的语句列表节点 */
StatementList* new_statement_list(ASTNode *statement) {
    StatementList *list = (StatementList*)malloc(sizeof(StatementList));
    if (!list) {
        AST_ERROR_EXIT("内存分配失败\n");
    }
    
    list->statement = statement;
    list->next = NULL;
    
    return list;
}

/* 创建新的表达式列表节点 */
ExpressionList* new_expression_list(ASTNode *expression) {
    ExpressionList *list = (ExpressionList*)malloc(sizeof(ExpressionList));
    if (!list) {
        AST_ERROR_EXIT("内存分配失败\n");
    }
    
    list->expression = expression;
    list->next = NULL;
    
    return list;
}

/* 创建新的成员列表节点 */
MemberList* new_member_list(const char* name) {
    MemberList* list = (MemberList*)malloc(sizeof(MemberList));
    list->next = NULL;
    list->decl = NULL;
    return list;
}

/* 释放成员列表内存 */
void free_member_list(MemberList *list) {
    MemberList *current = list;
    MemberList *next = NULL;
    
    while (current != NULL) {
        next = current->next;
        free_ast_node(current->decl);
        free(current);
        current = next;
    }
}

/* 释放AST节点内存 */
void free_parameter(Parameter *param) {
    if (param == NULL) return;
    
    free(param->name.name);
    if (param->default_value) free_ast_node(param->default_value);
    free(param);
}

void free_statement_list(StatementList *list) {
    if (list == NULL) return;
    
    free_ast_node(list->statement);
    free_statement_list(list->next);
    free(list);
}

void free_expression_list(ExpressionList *list) {
    if (list == NULL) return;
    
    free_ast_node(list->expression);
    free_expression_list(list->next);
    free(list);
}

void free_ast_node(ASTNode *node) {
    if (node == NULL) return;
    
    switch (node->type) {
        case AST_PROGRAM:
            free_statement_list(node->program.statements);
            break;
            
        case AST_VAR_DECL:
            free(node->var_decl.name.name);
            if (node->var_decl.value) free_ast_node(node->var_decl.value);
            break;
            
        case AST_FUNC_DECL:
            free(node->func_decl.name.name);
            free_parameter(node->func_decl.params);
            free_ast_node(node->func_decl.body);
            break;
            
        case AST_EXPR_STMT:
            free_ast_node(node->expr_stmt.expression);
            break;
            
        case AST_IF_STMT:
            free_ast_node(node->if_stmt.condition);
            free_ast_node(node->if_stmt.then_branch);
            free_ast_node(node->if_stmt.else_branch);
            break;
            
        case AST_FOR_STMT:
            free_ast_node(node->for_stmt.init);
            free_ast_node(node->for_stmt.condition);
            free_ast_node(node->for_stmt.increase);
            free_ast_node(node->for_stmt.body);
            break;
            
        case AST_WHILE_STMT:
            free_ast_node(node->while_stmt.condition);
            free_ast_node(node->while_stmt.body);
            break;
            
        case AST_RETURN_STMT:
            free_ast_node(node->return_stmt.expression);
            break;
            
        case AST_BLOCK_STMT:
            free_statement_list(node->block_stmt.statements);
            break;
            
        case AST_IMPORT_STMT:
            free(node->import_stmt.name);
            break;
            
        case AST_EXPORT_STMT:
            free(node->export_stmt.name);
            break;
            
        case AST_LITERAL_EXPR:
            free(node->literal_expr.value.name);
            break;
            
        case AST_IDENTIFIER_EXPR:
            free(node->identifier_expr.name);
            break;
            
        case AST_BINARY_EXPR:
            free_ast_node(node->binary_expr.left);
            free_ast_node(node->binary_expr.right);
            break;
            
        case AST_UNARY_EXPR:
            free_ast_node(node->unary_expr.operand);
            break;
            
        case AST_ASSIGNMENT_EXPR:
            free_ast_node(node->assignment_expr.left);
            free_ast_node(node->assignment_expr.right);
            break;
            
        case AST_CALL_EXPR:
            free_ast_node(node->call_expr.callee);
            free_expression_list(node->call_expr.arguments);
            break;
            
        case AST_ARRAY_ACCESS_EXPR:
            free_ast_node(node->array_access_expr.object);
            free_ast_node(node->array_access_expr.index);
            break;
            
        case AST_OBJECT_ACCESS_EXPR:
            free_ast_node(node->object_access_expr.object);
            free(node->object_access_expr.property.name);
            break;
            
        case AST_ANONYMOUS_FUNC_EXPR:
            free_parameter(node->anonymous_func_expr.params);
            free_ast_node(node->anonymous_func_expr.body);
            break;
        case AST_STRUCT_DECL:
        case AST_UNION_DECL:
            free(node->struct_or_union_decl.name.name);
            free_member_list(node->struct_or_union_decl.members);
            break;
        case AST_MEMBER_DECL:
            free(node->member_decl.name);
            break;
        default:
            break;
    }
    
    free(node);
}

const char* operator_to_string(Operator op) {
    if (op == OP_UNKNOWN) return "OP_UNKNOWN";
    
    // 算术运算符
    if (op == OP_PLUS) return "+";
    if (op == OP_MINUS) return "-";
    if (op == OP_STAR) return "*";
    if (op == OP_SLASH) return "/";
    if (op == OP_PERCENT) return "%";
    if (op == OP_POW) return "**";
    
    // 比较运算符
    if (op == OP_EQ) return "==";
    if (op == OP_NE) return "!=";
    if (op == OP_GT) return ">";
    if (op == OP_LT) return "<";
    if (op == OP_GE) return ">=";
    if (op == OP_LE) return "<=";
    
    // 逻辑运算符
    if (op == OP_AND) return "&&";
    if (op == OP_OR) return "||";
    if (op == OP_NOT) return "!";
    
    // 赋值运算符
    if (op == OP_ASSIGN) return "=";
    if (op == OP_ADD_ASSIGN) return "+=";
    if (op == OP_SUB_ASSIGN) return "-=";
    if (op == OP_MUL_ASSIGN) return "*=";
    if (op == OP_DIV_ASSIGN) return "/=";
    if (op == OP_MOD_ASSIGN) return "%=";
    
    // 无效操作符
    return "OP_INVALID";
}

/* 打印AST（用于调试） */
void print_member_list(MemberList *list, int depth) {
    MemberList *current = list;
    while (current != NULL) {
        print_ast(current->decl, depth);
        current = current->next;
    }
}

void print_ast(ASTNode *node, int depth) {
    if (node == NULL) return;
    
    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    
    switch (node->type) {
        case AST_PROGRAM:
            printf("PROGRAM\n");
            for (StatementList *stmt = node->program.statements; stmt; stmt = stmt->next) {
                print_ast(stmt->statement, depth + 1);
            }
            break;
            
        case AST_VAR_DECL:
            printf("VAR_DECL: %s (%s) :: %s\n", 
                   node->var_decl.name.name,
                   node->var_decl.var_type == VAR_TYPE_VARIABLE ? "令" : "恒",
                   type_to_string(node->inferred_type));
            if (node->var_decl.value) {
                print_ast(node->var_decl.value, depth + 1);
            }
            break;
            
        case AST_FUNC_DECL:
            printf("FUNC_DECL: %s :: %s\n", node->func_decl.name.name, type_to_string(node->inferred_type));
            
            for (int i = 0; i < depth + 1; i++) {
                printf("  ");
            }
            printf("PARAMS: ");
            
            int first = 1;
            for (Parameter *param = node->func_decl.params; param; param = param->next) {
                if (!first) printf(", ");
                printf("%s", param->name.name);
                if (param->default_value) {
                    printf("=");
                    print_ast(param->default_value, depth + 2);
                }
                first = 0;
            }
            printf("\n");
            
            print_ast(node->func_decl.body, depth + 1);
            break;
            
        case AST_EXPR_STMT:
            printf("EXPR_STMT\n");
            print_ast(node->expr_stmt.expression, depth + 1);
            break;
            
        case AST_IF_STMT: {
            // 判断是否是 elif 分支
            static int is_elif_context = 0;
            int was_elif_context = is_elif_context;
            
            if (!was_elif_context) {
                printf("IF_STMT\n");
            } else {
                printf("ELIF_STMT\n");
            }

            // 打印条件
            for(int i=0; i<depth; i++) printf("  "); 
            printf("  CONDITION:\n");
            print_ast(node->if_stmt.condition, depth + 2);
            
            // 打印 then 分支
            for(int i=0; i<depth; i++) printf("  "); 
            printf("  THEN_BRANCH:\n");
            print_ast(node->if_stmt.then_branch, depth + 2);
            
            // 处理 else/elif 分支
            if (node->if_stmt.else_branch) {
                if (node->if_stmt.else_branch->type == AST_IF_STMT) {
                    // 进入 elif 上下文
                    is_elif_context = 1;
                    print_ast(node->if_stmt.else_branch, depth);
                    is_elif_context = 0;
                } else {
                    for(int i=0; i<depth; i++) printf("  "); 
                    printf("ELSE_STMT:\n");
                    print_ast(node->if_stmt.else_branch, depth + 2);
                }
            }
            break;
        }
            
        case AST_FOR_STMT:
            printf("FOR_STMT\n");
            print_ast(node->for_stmt.init, depth + 1);
            print_ast(node->for_stmt.condition, depth + 1);
            print_ast(node->for_stmt.increase, depth + 1);
            print_ast(node->for_stmt.body, depth + 1);
            break;
            
        case AST_WHILE_STMT:
            printf("WHILE_STMT\n");
            print_ast(node->while_stmt.condition, depth + 1);
            print_ast(node->while_stmt.body, depth + 1);
            break;
            
        case AST_RETURN_STMT:
            printf("RETURN_STMT\n");
            if (node->return_stmt.expression) {
                print_ast(node->return_stmt.expression, depth + 1);
            }
            break;
            
        case AST_BLOCK_STMT:
            printf("BLOCK_STMT\n");
            for (StatementList *stmt = node->block_stmt.statements; stmt; stmt = stmt->next) {
                print_ast(stmt->statement, depth + 1);
            }
            break;
            
        case AST_IMPORT_STMT:
            printf("IMPORT_STMT: %s\n", node->import_stmt.name);
            break;
            
        case AST_EXPORT_STMT:
            printf("EXPORT_STMT: %s\n", node->export_stmt.name);
            break;
            
        case AST_LITERAL_EXPR:
            printf("LITERAL_EXPR: %s :: %s\n",
                    node->literal_expr.value.name,
                    type_to_string(node->inferred_type));
            break;
            
        case AST_IDENTIFIER_EXPR:
            printf("IDENTIFIER_EXPR: %s :: %s\n", node->identifier_expr.name, type_to_string(node->inferred_type));
            break;
            
        case AST_BINARY_EXPR:
            printf("BINARY_EXPR: %s :: %s\n", operator_to_string(node->binary_expr.op), 
                                type_to_string(node->inferred_type));
            print_ast(node->binary_expr.left, depth + 1);
            print_ast(node->binary_expr.right, depth + 1);
            break;
            
        case AST_UNARY_EXPR:
            printf("UNARY_EXPR: %s :: %s\n", operator_to_string(node->unary_expr.op), 
                          type_to_string(node->inferred_type));
            print_ast(node->unary_expr.operand, depth + 1);
            break;
            
        case AST_ASSIGNMENT_EXPR:
            printf("ASSIGNMENT_EXPR: %s :: %s\n", operator_to_string(node->assignment_expr.op), 
                     type_to_string(node->inferred_type));
            print_ast(node->assignment_expr.left, depth + 1);
            print_ast(node->assignment_expr.right, depth + 1);
            break;
            
        case AST_CALL_EXPR:
            printf("CALL_EXPR :: %s\n", type_to_string(node->inferred_type));
            print_ast(node->call_expr.callee, depth + 1);
            
            for (int i = 0; i < depth + 1; i++) {
                printf("  ");
            }
            printf("ARGUMENTS:\n");
            
            for (ExpressionList *arg = node->call_expr.arguments; arg; arg = arg->next) {
                print_ast(arg->expression, depth + 2);
            }
            break;
            
        case AST_ARRAY_ACCESS_EXPR:
            printf("ARRAY_ACCESS_EXPR :: %s\n", type_to_string(node->inferred_type));
            print_ast(node->array_access_expr.object, depth + 1);
            print_ast(node->array_access_expr.index, depth + 1);
            break;
            
        case AST_OBJECT_ACCESS_EXPR:
            printf("OBJECT_ACCESS_EXPR %s :: %s\n", node->object_access_expr.property.name, type_to_string(node->inferred_type));
            print_ast(node->object_access_expr.object, depth + 1);
            break;
            
        case AST_ANONYMOUS_FUNC_EXPR:
            printf("ANONYMOUS_FUNC_EXPR :: %s\n", type_to_string(node->inferred_type));
            
            for (int i = 0; i < depth + 1; i++) {
                printf("  ");
            }
            printf("PARAMS: ");
            
            first = 1;
            for (Parameter *param = node->anonymous_func_expr.params; param; param = param->next) {
                if (!first) printf(", ");
                printf("%s", param->name.name);
                if (param->default_value) {
                    printf("=");
                    print_ast(param->default_value, depth + 2);
                }
                first = 0;
            }
            printf("\n");
            
            print_ast(node->anonymous_func_expr.body, depth + 1);
            break;
        case AST_CONTINUE_STMT:
            printf("CONTINUE_STMT\n");
            break;
        case AST_BREAK_STMT:
            printf("BREAK_STMT\n");
            break;
        case AST_STRUCT_DECL:
            printf("STRUCT_DECL: %s :: %s\n", node->struct_or_union_decl.name.name, type_to_string(node->inferred_type));
            print_member_list(node->struct_or_union_decl.members, depth + 1);
            break;
            
        case AST_UNION_DECL:
            printf("UNION_DECL: %s :: %s\n", node->struct_or_union_decl.name.name, type_to_string(node->inferred_type));
            print_member_list(node->struct_or_union_decl.members, depth + 1);
            break;
            
        case AST_MEMBER_DECL:
            printf("MEMBER_DECL: %s :: %s\n", node->member_decl.name, type_to_string(node->inferred_type));
            break;
    }
}