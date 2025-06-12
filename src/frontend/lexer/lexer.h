#ifndef LEXER_H
#define LEXER_H

/* 定义词法单元类型*/
typedef enum {
    TOKEN_INIT = 0,
    /**标识符 */
    TOKEN_IDENTIFIER = 1,
    /**关键字 */
    TOKEN_VAR = 2,
    TOKEN_CONST = 3,
    TOKEN_FUNC = 4,
    TOKEN_RETURN = 5,
    TOKEN_IF = 6,
    TOKEN_ELIF = 7,
    TOKEN_ELSE = 8,
    TOKEN_FOR = 9,
    TOKEN_WHILE = 10,
    TOKEN_BREAK = 11,
    TOKEN_CONTINUE = 12,
    TOKEN_IMPORT = 13,
    TOKEN_EXPORT = 14,
    TOKEN_STRUCT = 15,
    TOKEN_UNION = 16,
    TOKEN_BOOLEAN = 17,
    TOKEN_NULL = 18,
    /**数字 */
    TOKEN_NUMBER = 19,
    /**字符串 */
    TOKEN_STRING = 20,
    /**字符 */
    TOKEN_CHAR = 21,
    /**操作符 */
    // 算术运算符
    TOKEN_PLUS = 22,      // +
    TOKEN_MINUS = 23,     // -
    TOKEN_STAR = 24,      // *
    TOKEN_SLASH = 25,     // /
    TOKEN_PERCENT = 26,   // %
    TOKEN_POW = 27,       // **
    // 比较运算符
    TOKEN_EQ = 28,        // ==
    TOKEN_NE = 29,        // !=
    TOKEN_GT = 30,        // >
    TOKEN_LT = 31,        // <
    TOKEN_GE = 32,        // >=
    TOKEN_LE = 33,        // <=
    // 逻辑运算符
    TOKEN_AND = 34,       // &&
    TOKEN_OR = 35,        // ||
    TOKEN_NOT = 36,       // !
    // 赋值运算符
    TOKEN_ASSIGN = 37,    // =
    TOKEN_ADD_ASSIGN = 38, // +=
    TOKEN_SUB_ASSIGN = 39, // -=
    TOKEN_MUL_ASSIGN = 40, // *=
    TOKEN_DIV_ASSIGN = 41, // /=
    TOKEN_MOD_ASSIGN = 42, // %=
    /**分隔符 */
    TOKEN_LEFT_PAREN = 43,     // (
    TOKEN_RIGHT_PAREN = 44,    // )
    TOKEN_LEFT_BRACKET = 45,   // [
    TOKEN_RIGHT_BRACKET = 46,  // ]
    TOKEN_LEFT_BRACE = 47,     // {
    TOKEN_RIGHT_BRACE = 48,    // }
    TOKEN_COMMA = 49,          // ,
    TOKEN_SEMICOLON = 50,      // ;
    TOKEN_COLON = 51,          // :
    TOKEN_ARROW = 52,          // =>
    TOKEN_SPREAD = 53,         // ...
    TOKEN_DOT = 54,            // .
    /**文件结束 */
    TOKEN_EOF = 55,
    TOKEN_MAX
} TokenType;

/* 定义词法单元结构 */
typedef struct {
    TokenType type;
    char *value;
    int line;
} Token;

/* 定义词法分析器结构 */
typedef struct {
    const char *input;
    int position;
    int line;
} Lexer;

/* 函数声明 */
Lexer* init_lexer(const char *input);
Token* get_next_token(Lexer *lexer);
void free_token(Token *token);
void free_lexer(Lexer *lexer);

const char* token_type_to_string(TokenType type);
#endif /* LEXER_H */    