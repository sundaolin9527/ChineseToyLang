#ifndef PARSER_H
#define PARSER_H

#include "../lexer/lexer.h"
#include "../ast/ast.h"

/* 解析器结构 */
typedef struct {
    Lexer *lexer;
    Token *current_token;
} Parser;

/* 函数声明 */
Parser* init_parser(Lexer *lexer);
ASTNode* parse_program(Parser *parser);

#endif /* PARSER_H */    