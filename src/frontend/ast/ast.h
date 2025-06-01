#ifndef AST_H
#define AST_H

#include "../lexer/lexer.h"

/* 抽象语法树节点类型 */
typedef enum {
    AST_PROGRAM,
    AST_VAR_DECL, 
    AST_FUNC_DECL, 
    AST_EXPR_STMT,
    AST_IF, AST_FOR, 
    AST_WHILE, 
    AST_BREAK,
    AST_CONTINUE,
    AST_RETURN,
    AST_BLOCK, 
    AST_IMPORT, 
    AST_EXPORT,
    AST_LITERAL, 
    AST_IDENTIFIER, 
    AST_BINARY_EXPR,
    AST_UNARY_EXPR, 
    AST_ASSIGNMENT, 
    AST_CALL,
    AST_ARRAY_ACCESS, 
    AST_OBJECT_ACCESS, 
    AST_ANONYMOUS_FUNC,
    AST_STRUCT_DECL,
    AST_UNION_DECL,
    AST_MEMBER_DECL,       // 普通成员
} ASTNodeType;

/* 变量声明类型 */
typedef enum {
    VAR_TYPE_VARIABLE, VAR_TYPE_CONSTANT
} VarDeclarationType;

/* 字面量类型 */
typedef enum {
    LITERAL_NUMBER, LITERAL_STRING, LITERAL_CHAR,
    LITERAL_BOOLEAN, LITERAL_NULL
} LiteralType;

/* AST节点结构 */
typedef struct ASTNode ASTNode;
typedef struct Parameter Parameter;
typedef struct StatementList StatementList;
typedef struct ExpressionList ExpressionList;
typedef struct MemberList MemberList;

/* 参数结构 */
struct Parameter {
    char *name;
    char *type;  // 类型注解
    ASTNode *default_value;  // 默认值表达式
    Parameter *next;
};

/* 语句列表 */
struct StatementList {
    ASTNode *statement;
    StatementList *next;
};

/* 表达式列表 */
struct ExpressionList {
    ASTNode *expression;
    ExpressionList *next;
};

/* 结构体或联合体成员列表 */
struct MemberList {
    ASTNode *decl; // 嵌套的结构体/联合体（可为NULL）
    MemberList *next;  // 成员列表
};

/* AST节点 */
struct ASTNode {
    ASTNodeType type;
    int line;
    int column;
    
    union {
        /* 程序节点 */
        struct {
            StatementList *statements;
        } program;
        
        /* 变量声明节点 */
        struct {
            VarDeclarationType var_type;
            char *name;
            ASTNode *value;
        } var_decl;
        
        /* 函数声明节点 */
        struct {
            char *name;
            Parameter *params;
            ASTNode *body;  // 块语句
        } func_decl;
        
        /* 表达式语句节点 */
        struct {
            ASTNode *expression;
        } expr_stmt;
        
        /* 条件语句节点 */
        struct {
            ASTNode *condition;
            ASTNode *then_branch;
            ASTNode *else_branch;
        } if_stmt;
        
        /* for循环节点 */
        struct {
            ASTNode *init;
            ASTNode *condition;
            ASTNode *update;
            ASTNode *body;
        } for_loop;
        
        /* while循环节点 */
        struct {
            ASTNode *condition;
            ASTNode *body;
        } while_loop;
        
        /* 返回语句节点 */
        struct {
            ASTNode *value;
        } return_stmt;
        
        /* 块语句节点 */
        struct {
            StatementList *statements;
        } block;
        
        /* 导入语句节点 */
        struct {
            char *module;
        } import;
        
        /* 导出语句节点 */
        struct {
            char *name;
        } export;
        
        /* 字面量节点 */
        struct {
            LiteralType literal_type;
            char *value;
        } literal;
        
        /* 标识符节点 */
        struct {
            char *name;
        } identifier;
        
        /* 二元表达式节点 */
        struct {
            char *operator;
            ASTNode *left;
            ASTNode *right;
        } binary_expr;
        
        /* 一元表达式节点 */
        struct {
            char *operator;
            ASTNode *operand;
        } unary_expr;
        
        /* 赋值表达式节点 */
        struct {
            char *operator;
            ASTNode *left;
            ASTNode *right;
        } assignment;
        
        /* 函数调用节点 */
        struct {
            ASTNode *callee;
            ExpressionList *arguments;
        } call;
        
        /* 数组访问节点 */
        struct {
            ASTNode *object;
            ASTNode *index;
        } array_access;
        
        /* 对象访问节点 */
        struct {
            ASTNode *object;
            char *property;
        } object_access;
        
        /* 匿名函数节点 */
        struct {
            Parameter *params;
            ASTNode *body;  // 表达式或块语句
        } anonymous_func;

        struct {
            char *name;     // 成员名
            char *type;     // 类型（可为NULL）
        } member_decl;

        /* 结构体或结构体声明节点 */
        struct {
            char *name; // 可为NULL(匿名)
            MemberList *members;
        } struct_or_union_decl;
    };
};

ASTNode* new_ast_node(ASTNodeType type, int line, int column);
Parameter* new_parameter(const char *name, const char *type, ASTNode *default_value);
StatementList* new_statement_list(ASTNode *statement);
ExpressionList* new_expression_list(ASTNode *expression);
MemberList* new_member_list(const char* name);
void free_member_list(MemberList* list);

void free_ast_node(ASTNode *node);
void print_ast(ASTNode *node, int depth);
#endif // AST_H