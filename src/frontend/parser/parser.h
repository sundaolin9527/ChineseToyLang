#ifndef PARSER_H
#define PARSER_H

#include "../lexer/lexer.h"
#include "../ast/ast.h"
#include "../type/type.h"
#include "../infer/infer.h"

/* 解析器结构 */
typedef struct {
    TypeEnv *env;
    Lexer *lexer;
    Token *current_token;
} Parser;

/* 函数声明 */
Parser* init_parser(Lexer *lexer);
ASTNode* parse_program(Parser *parser);
void free_parser(Parser **pparser);
#endif /* PARSER_H */