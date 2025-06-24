#ifndef PARSER_H
#define PARSER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "Frontend/Lexer.h"
#include "Frontend/Ast.h"
#include "Frontend/Infer.h"
#include "Basic/Types.h"
#include "Basic/Utils.h"

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
#ifdef __cplusplus
}
#endif
#endif /* PARSER_H */
