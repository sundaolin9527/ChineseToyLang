#ifndef AST_H
#define AST_H

#include "../lexer/lexer.h"
#include "../type/type.h"

/* 抽象语法树节点类型 */
typedef enum {
    // ========== 程序结构 ==========
    AST_PROGRAM,       // 程序根节点，包含所有顶层语句（如函数、变量声明等）

    // ========== 声明类节点 ==========
    AST_VAR_DECL,      // 变量声明，如 `x = 10;` 或 `y;`
    AST_FUNC_DECL,     // 函数声明，如 `function foo() { ... }`
    AST_STRUCT_DECL,   // 结构体声明，如 `struct Point { x; y; }`
    AST_UNION_DECL,    // 联合体声明，如 `union Data { i; f; }`
    AST_MEMBER_DECL,   // 结构体/联合体的成员声明，如 `x`（在 `struct` 内部使用）

    // ========== 语句类节点 ==========
    AST_BLOCK_STMT,    // 代码块 `{ ... }`，包含一组语句（语句块）
    AST_EXPR_STMT,     // 表达式语句，如 `x + 1;`（表达式后加分号）
    AST_IF_STMT,       // `if` 条件语句，如 `if (cond) { ... } elseif (cond) {} else { ... }`
    AST_FOR_STMT,      // `for` 循环，如 `for (i = 0; i < 10; i++) { ... }`
    AST_WHILE_STMT,    // `while` 循环，如 `while (cond) { ... }`
    AST_BREAK_STMT,    // `break` 语句，用于跳出循环
    AST_CONTINUE_STMT, // `continue` 语句，用于跳过当前循环迭代
    AST_RETURN_STMT,   // `return` 语句，如 `return x + 1;`
    AST_IMPORT_STMT,   // `import` 语句，如 `import "module"`
    AST_EXPORT_STMT,   // `export` 语句，如 `export function foo() { ... }`

    // ========== 表达式类节点（可计算值） ==========
    AST_LITERAL_EXPR,      // 字面量，如 `42`, `3.14`, `"hello"`, `true`
    AST_IDENTIFIER_EXPR,   // 标识符（变量名），如 `x`, `myVar`
    AST_BINARY_EXPR,       // 二元运算，如 `a + b`, `x > y`, `a && b`
    AST_UNARY_EXPR,        // 一元运算，如 `-x`, `!flag`, `++i`
    AST_ASSIGNMENT_EXPR,   // 赋值，如 `x = 10`（可能返回赋值后的值）
    AST_CALL_EXPR,         // 函数调用，如 `foo(1, 2)`
    AST_ARRAY_ACCESS_EXPR, // 数组访问，如 `arr[0]`
    AST_OBJECT_ACCESS_EXPR,// 对象/结构体字段访问，如 `obj.field`
    AST_ANONYMOUS_FUNC_EXPR // 匿名函数/闭包，如 `(x) => x + 1`
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
    Type inferred_type;
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