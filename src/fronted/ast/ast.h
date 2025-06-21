#ifndef AST_H
#define AST_H
#ifdef __cplusplus
extern "C" {
#endif
#include "fronted/lexer/lexer.h"
#include "fronted/type/type.h"

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
} VarDeclType;

/* 字面量类型 */
typedef enum {
    LITERAL_UNKNOWN = 0,
    LITERAL_INT8, 
    LITERAL_INT16, 
    LITERAL_INT32,
    LITERAL_INT64,
    LITERAL_UINT8, 
    LITERAL_UINT16, 
    LITERAL_UINT32, 
    LITERAL_UINT64,
    LITERAL_FLOAT16, 
    LITERAL_FLOAT32, 
    LITERAL_FLOAT64,
    LITERAL_STRING,
    LITERAL_CHAR, 
    LITERAL_BOOLEAN, 
    LITERAL_NULL
} LiteralType;

// 操作符
typedef enum {
    OP_UNKNOWN = 0,
    // 算术运算符
    OP_PLUS,        // +
    OP_MINUS,       // -
    OP_STAR,        // *
    OP_SLASH,       // /
    OP_PERCENT,     // %
    OP_POW,         // **
    
    // 比较运算符
    OP_EQ,          // ==
    OP_NE,          // !=
    OP_GT,          // >
    OP_LT,          // <
    OP_GE,          // >=
    OP_LE,          // <=
    
    // 逻辑运算符
    OP_AND,         // &&
    OP_OR,          // ||
    OP_NOT,         // !
    
    // 赋值运算符
    OP_ASSIGN,      // =
    OP_ADD_ASSIGN,  // +=
    OP_SUB_ASSIGN,  // -=
    OP_MUL_ASSIGN,  // *=
    OP_DIV_ASSIGN,  // /=
    OP_MOD_ASSIGN   // %=
} Operator;

/* ====================== 前置声明 ====================== */
typedef struct ASTNode ASTNode;
typedef struct Parameter Parameter;
typedef struct StatementList StatementList;
typedef struct ExpressionList ExpressionList;
typedef struct MemberList MemberList;

typedef struct StmtSequence StmtSequence;
typedef struct VarDecl VarDecl;
typedef struct FunctionDecl FunctionDecl;
typedef struct StructOrUnionDecl StructOrUnionDecl;
typedef struct IfStmt IfStmt;
typedef struct ForStmt ForStmt;
typedef struct WhileStmt WhileStmt;
typedef struct SimpleStmt SimpleStmt;
typedef struct LiteralExpr LiteralExpr;
typedef struct BinaryExpr BinaryExpr;
typedef struct UnaryExpr UnaryExpr;
typedef struct CallExpr CallExpr;
typedef struct ArrayAccessExpr ArrayAccessExpr;
typedef struct ObjectAccessExpr ObjectAccessExpr;
typedef struct AnoymousFuncExpr AnoymousFuncExpr;
typedef struct Name Name;
/* ====================== 基本类型定义 ====================== */
typedef struct Name{
    char* name;
} Name;

/* 参数结构 - 用于函数参数和匿名函数参数 */
typedef struct Parameter {
    Name name;                  // 参数名称
    ASTNode* default_value;     // 默认值表达式 (可为NULL)
    int para_cnt;               // 参数个数 (仅在链表头节点有效)
    Parameter* next;            // 下一个参数
} Parameter;

/* 语句链表节点 - 用于代码块和程序根节点 */
typedef struct StatementList {
    ASTNode* statement;         // 语句节点
    StatementList* next;        // 下一条语句
} StatementList;

/* 表达式链表节点 - 用于函数调用参数等 */
typedef struct ExpressionList {
    ASTNode* expression;        // 表达式节点
    ExpressionList* next;       // 下一个表达式
} ExpressionList;

/* 成员链表节点 - 用于结构体和联合体成员 */
typedef struct MemberList {
    ASTNode* decl;              // 成员声明节点 (可为NULL)
    MemberList* next;           // 下一个成员
} MemberList;

/* ====================== AST 节点数据定义 ====================== */
typedef struct StmtSequence{
    StatementList* statements;
} StmtSequence;

/* 变量声明节点数据 */
typedef struct VarDecl{
    Name name;  // 变量名
    VarDeclType var_type; // 变量或常量
    ASTNode* value;     // 初始值表达式 (可为NULL)
} VarDecl;

/* 结构体或结构体声明节点 */
typedef struct StructOrUnionDecl{
    Name name;  // 可为NULL(匿名)
    MemberList *members;
} StructOrUnionDecl;

/* 函数声明节点数据 */
typedef struct FunctionDecl{
    Name name;                  // 函数名
    Parameter* params;          // 参数列表
    ASTNode* body;              // 函数体 (块语句)
} FunctionDecl;

/* 控制结构节点数据 */
typedef struct IfStmt{
    ASTNode* condition;         // 条件表达式
    ASTNode* then_branch;       // then分支
    ASTNode* else_branch;       // else分支 (可为NULL)
} IfStmt;

typedef struct ForStmt{
    ASTNode* init;              // 初始化表达式 (可为NULL)
    ASTNode* condition;         // 循环条件
    ASTNode* update;           // 更新表达式 (可为NULL)
    ASTNode* body;             // 循环体
} ForStmt;

typedef struct WhileStmt{
    ASTNode* condition;         // 循环条件
    ASTNode* body;             // 循环体
} WhileStmt;

typedef struct SimpleStmt{
    ASTNode* expression;  // 表达式或返回值
} SimpleStmt;

/* 函数调用节点 */
typedef struct CallExpr{
    ASTNode *callee;
    ExpressionList *arguments;
} CallExpr;

/* 数组访问节点 */
typedef struct ArrayAccessExpr{
    ASTNode *object;
    ASTNode *index;
} ArrayAccessExpr;

/* 对象访问节点 */
typedef struct ObjectAccessExpr{
    ASTNode *object;
    Name property;
} ObjectAccessExpr;

/* 匿名函数节点 */
typedef struct AnoymousFuncExpr{
    Parameter *params;
    ASTNode *body;  // 表达式或块语句
} AnoymousFuncExpr;

/* 表达式相关节点数据 */
typedef struct LiteralExpr{
    LiteralType literal_type;   // 字面量类型
    Name value;                 // 字面量值字符串
} LiteralExpr;

typedef struct BinaryExpr{
    Operator op;               // 操作符
    ASTNode* left;             // 左操作数
    ASTNode* right;            // 右操作数
} BinaryExpr;

typedef struct UnaryExpr{
    Operator op;               // 操作符
    ASTNode* operand;          // 操作数
} UnaryExpr;

/* ====================== AST 节点主结构 ====================== */
/* AST 节点主结构 */
struct ASTNode {
    int line;                  // 源代码行号
    ASTNodeType type;          // 节点类型
    ValueType inferred_type;   // 推断的类型
    union {
        StmtSequence program;
        VarDecl var_decl;
        FunctionDecl func_decl;
        StructOrUnionDecl struct_or_union_decl;
        Name member_decl;
        IfStmt if_stmt;
        ForStmt for_stmt;
        WhileStmt while_stmt;
        SimpleStmt expr_stmt;
        Name import_stmt;
        Name export_stmt;
        SimpleStmt return_stmt;
        StmtSequence block_stmt;
        LiteralExpr literal_expr;
        Name identifier_expr;
        BinaryExpr binary_expr;
        UnaryExpr unary_expr;
        BinaryExpr assignment_expr;
        CallExpr call_expr;
        ArrayAccessExpr array_access_expr;
        ObjectAccessExpr object_access_expr;
        AnoymousFuncExpr anonymous_func_expr;
    };
};

Operator token_to_operator(TokenType token);
ASTNode* new_ast_node(ASTNodeType type, int line);
Parameter* new_parameter(const char *name, ASTNode *default_value);
StatementList* new_statement_list(ASTNode *statement);
ExpressionList* new_expression_list(ASTNode *expression);
MemberList* new_member_list(const char* name);
void free_member_list(MemberList* list);

void free_ast_node(ASTNode *node);
void print_ast(ASTNode *node, int depth);

#ifdef __cplusplus
}
#endif
#endif // AST_H
