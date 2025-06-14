#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parser.h"
#include "../../utils/utils.h"

#define PARSER_ERROR_EXIT(fmt, ...) \
    do { \
        fprintf(stderr, "[语法错误] " fmt " (%s:%d)\n", ##__VA_ARGS__, __FILE__, __LINE__); \
        exit(EXIT_FAILURE); \
    } while (0)

#define SYNTAX_ERROR_EXIT(parser, expected_str, expected_token_type) \
    do { \
        if (!(parser) || !(parser)->current_token) { \
            fprintf(stderr, "[语法错误] %s:%d: 无效的Parser或Token\n", __FILE__, __LINE__); \
            exit(EXIT_FAILURE); \
        } \
        Token* tok = (parser)->current_token; \
        const char *expect = token_type_to_string(expected_token_type); \
        const char* actual = token_type_to_string(tok->type); \
        fprintf(stderr, "[语法错误] %s:%d, 被解析的源文件 %d 行有错误,\n[语法错误] 期望%s: %s, 但得到 %s\n", \
               __FILE__, __LINE__, tok->line, expected_str, expect, actual); \
        exit(EXIT_FAILURE); \
    } while (0)

ASTNode* parse_statement(Parser *parser);
ASTNode* parse_func_declaration(Parser *parser);
Parameter* parse_parameter_list(Parser *parser);
ASTNode* parse_expression(Parser *parser);
ASTNode* parse_assignment_expression(Parser *parser);
ASTNode* parse_logical_or_expression(Parser *parser);
ASTNode* parse_logical_and_expression(Parser *parser);
ASTNode* parse_equality_expression(Parser *parser);
ASTNode* parse_comparison_expression(Parser *parser);
ASTNode* parse_additive_expression(Parser *parser);
ASTNode* parse_multiplicative_expression(Parser *parser);
ASTNode* parse_exponential_expression(Parser *parser);
ASTNode* parse_unary_expression(Parser *parser);
ASTNode* parse_primary_expression(Parser *parser);
ExpressionList* parse_expression_list(Parser *parser);
ASTNode* parse_struct_or_union(Parser* parser);

/* 初始化解析器 */
Parser* init_parser(Lexer *lexer) {
    Parser *parser = (Parser*)malloc(sizeof(Parser));
    if (!parser) {
        PARSER_ERROR_EXIT("内存分配失败\n");
    }
    
    parser->lexer = lexer;
    parser->current_token = get_next_token(lexer);
    parser->env = init_type_env();
    return parser;
}

void free_parser(Parser **pparser) {
    if (!pparser || !(*pparser)) return;

    Parser *parser = *pparser;
    if (parser->env)
    {
        free_type_env(&parser->env);
    }

    if (parser->current_token)
    {
        free(parser->current_token);
        parser->current_token = NULL;
    }

    if (parser->lexer)
    {
        free(parser->lexer);
        parser->lexer = NULL;
    }
    free(parser);
    *pparser = NULL;
    return;
}
/* 消耗当前token并获取下一个token */
void eat(Parser *parser, TokenType token_type) {
    if (parser->current_token->type == token_type) {
        Token *old_token = parser->current_token;
        parser->current_token = get_next_token(parser->lexer);
        free_token(old_token);
    } else {
        SYNTAX_ERROR_EXIT(parser, "", token_type);
    }
}

/* 解析程序 */
ASTNode* parse_program(Parser *parser) {
    enter_scope(parser->env);
    ASTNode *node = new_ast_node(AST_PROGRAM, 0);
    node->program.statements = NULL;
    StatementList *tail = NULL;
    ASTNode *statement = NULL;
    while (parser->current_token->type != TOKEN_EOF) {
        statement = parse_statement(parser);
        
        if (!node->program.statements) {
            node->program.statements = new_statement_list(statement);
            tail = node->program.statements;
        } else {
            tail->next = new_statement_list(statement);
            tail = tail->next;
        }
    }
    exit_scope(parser->env);
    return node;
}

/* 解析变量声明 */
ASTNode* parse_var_declaration(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_VAR_DECL, line);

    TokenType tokenType = parser->current_token->type;
    if (tokenType == TOKEN_VAR) {
        node->var_decl.var_type = VAR_TYPE_VARIABLE;
    } else if(tokenType == TOKEN_CONST) {
        node->var_decl.var_type = VAR_TYPE_CONSTANT;
    }
    else
    {
        SYNTAX_ERROR_EXIT(parser, "变量或常量", tokenType);
    }
    eat(parser, tokenType);
    
    if (parser->current_token->type != TOKEN_IDENTIFIER) {
        SYNTAX_ERROR_EXIT(parser, "标识符", TOKEN_IDENTIFIER);
    }
    node->var_decl.name = strdup(parser->current_token->value);
    eat(parser, TOKEN_IDENTIFIER);
    
    node->var_decl.value = NULL;
    if (parser->current_token->type == TOKEN_ASSIGN) {
        eat(parser, TOKEN_ASSIGN);
        node->var_decl.value = parse_expression(parser);
    }
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    node->inferred_type = infer_type(parser->env, node);
    add_symbol_to_scope(parser->env, node->var_decl.name, node->inferred_type);
    return node;
}

/* 解析函数声明 */
ASTNode* parse_func_declaration(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_FUNC_DECL, line);
    
    eat(parser, TOKEN_FUNC);  // 消耗"函"
    
    if (parser->current_token->type != TOKEN_IDENTIFIER) {
        SYNTAX_ERROR_EXIT(parser, "函数名", TOKEN_IDENTIFIER);
    }
    node->func_decl.name = strdup(parser->current_token->value);
    eat(parser, TOKEN_IDENTIFIER);
    
    if (parser->current_token->type != TOKEN_LEFT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "左括号", TOKEN_LEFT_PAREN);
    }
    eat(parser, TOKEN_LEFT_PAREN);
    
    node->func_decl.params = NULL;
    if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
        node->func_decl.params = parse_parameter_list(parser);
    }
    
    if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "右括号", TOKEN_RIGHT_PAREN);
    }
    eat(parser, TOKEN_RIGHT_PAREN);
    
    node->func_decl.body = parse_statement(parser);
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析参数列表 */
Parameter* parse_parameter_list(Parser *parser) {
    Parameter *head = NULL;
    Parameter *tail = NULL;
    ASTNode *default_value = NULL;
    Parameter *param = NULL;

    do {
        if (parser->current_token->type != TOKEN_IDENTIFIER) {
            SYNTAX_ERROR_EXIT(parser, "参数名", TOKEN_IDENTIFIER);
        }
        
        char *name = strdup(parser->current_token->value);
        eat(parser, TOKEN_IDENTIFIER);
        
        char *type = NULL;
        if (parser->current_token->type == TOKEN_ASSIGN) {
            eat(parser, TOKEN_ASSIGN);
            default_value = parse_expression(parser);
        }
        
        param = new_parameter(name, default_value);
        
        if (!head) {
            head = param;
            tail = param;
        } else {
            tail->next = param;
            tail = param;
        }
        head->para_cnt++;

        if (parser->current_token->type != TOKEN_COMMA) {
            break;
        }
        eat(parser, TOKEN_COMMA);
    } while (1);
    
    return head;
}

/* 解析if语句 */
ASTNode* parse_if_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_IF_STMT, line);
    
    eat(parser, TOKEN_IF);  // 消耗"若"
    
    // 解析条件表达式
    if (parser->current_token->type != TOKEN_LEFT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "左括号", TOKEN_LEFT_PAREN);
    }
    eat(parser, TOKEN_LEFT_PAREN);
    node->if_stmt.condition = parse_expression(parser);
    eat(parser, TOKEN_RIGHT_PAREN);
    
    // 解析 then 分支
    node->if_stmt.then_branch = parse_statement(parser);
    
    // 处理 elseif 和 else 分支
    ASTNode *current = node; // 跟踪当前处理的if/elif节点
    
    while (parser->current_token->type == TOKEN_ELSE || 
           parser->current_token->type == TOKEN_ELIF) {
        
        if (parser->current_token->type == TOKEN_ELIF) {
            // 创建新的 elif 节点
            ASTNode *elif_node = new_ast_node(AST_IF_STMT, 
                parser->current_token->line);
            
            eat(parser, TOKEN_ELIF);  // 消耗"亦"
            
            // 解析 elseif 条件
            if (parser->current_token->type != TOKEN_LEFT_PAREN) {
                SYNTAX_ERROR_EXIT(parser, "左括号", TOKEN_LEFT_PAREN);
            }
            eat(parser, TOKEN_LEFT_PAREN);
            elif_node->if_stmt.condition = parse_expression(parser);
            eat(parser, TOKEN_RIGHT_PAREN);
            
            // 解析 elseif 的 then 分支
            elif_node->if_stmt.then_branch = parse_statement(parser);
            elif_node->if_stmt.else_branch = NULL;
            
            // 将 elif 节点连接到当前节点的 else 分支
            current->if_stmt.else_branch = elif_node;
            current = elif_node; // 更新当前节点为新创建的 elif 节点
            
            // 继续检查是否还有更多 elseif
            continue;
        }
        
        // 处理 else 分支
        if (parser->current_token->type == TOKEN_ELSE) {
            eat(parser, TOKEN_ELSE);  // 消耗"则"
            
            // 确保 else 分支连接到最内层未处理的 if/elif
            current->if_stmt.else_branch = parse_statement(parser);
            break;  // else 必须是最后一个分支
        }
    }
    
    return node;
}

/* 解析for语句 */
ASTNode* parse_for_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_FOR_STMT, line);
    
    eat(parser, TOKEN_FOR);  // 消耗"遍"
    
    if (parser->current_token->type != TOKEN_LEFT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "左括号", TOKEN_LEFT_PAREN);
    }
    eat(parser, TOKEN_LEFT_PAREN);
    
    // 初始化部分
    if (parser->current_token->type == TOKEN_VAR ||
        parser->current_token->type == TOKEN_CONST) 
    {
        node->for_loop.init = parse_var_declaration(parser);
    } else if (parser->current_token->type != TOKEN_SEMICOLON) {
        node->for_loop.init = parse_expression(parser);
        
        if (parser->current_token->type != TOKEN_SEMICOLON) {
            SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
        }
        eat(parser, TOKEN_SEMICOLON);
    } else {
        eat(parser, TOKEN_SEMICOLON);  // 消耗分号
    }
    
    // 条件部分
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        node->for_loop.condition = parse_expression(parser);
    } else {
        node->for_loop.condition = NULL;
    }
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    // 更新部分
    if (parser->current_token->type != TOKEN_LEFT_PAREN) {
        node->for_loop.update = parse_expression(parser);
    } else {
        node->for_loop.update = NULL;
    }
    
    if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "右括号", TOKEN_RIGHT_PAREN);
    }
    eat(parser, TOKEN_RIGHT_PAREN);
    
    node->for_loop.body = parse_statement(parser);
    
    return node;
}

/* 解析while语句 */
ASTNode* parse_while_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_WHILE_STMT, line);
    
    eat(parser, TOKEN_WHILE);  // 消耗"循环"
    
    if (parser->current_token->type != TOKEN_LEFT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "左括号", TOKEN_LEFT_PAREN);
    }
    eat(parser, TOKEN_LEFT_PAREN);
    
    node->while_loop.condition = parse_expression(parser);
    
    if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
        SYNTAX_ERROR_EXIT(parser, "右括号", TOKEN_RIGHT_PAREN);
    }
    eat(parser, TOKEN_RIGHT_PAREN);
    
    node->while_loop.body = parse_statement(parser);
    
    return node;
}

/* 解析break语句 */
ASTNode* parse_break_statement(Parser *parser) {
    int line = parser->current_token->line;

    ASTNode *node = new_ast_node(AST_BREAK_STMT, line);
    
    eat(parser, TOKEN_BREAK);  // 消耗"撤"
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    return node;
}

/* 解析continue语句 */
ASTNode* parse_continue_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_CONTINUE_STMT, line);
    
    eat(parser, TOKEN_CONTINUE);  // 消耗"过"
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    return node;
}

/* 解析return语句 */
ASTNode* parse_return_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_RETURN_STMT, line);
    
    eat(parser, TOKEN_RETURN);  // 消耗"返"
    
    node->return_stmt.value = NULL;
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        node->return_stmt.value = parse_expression(parser);
    }
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    return node;
}

/* 解析块语句 */
ASTNode* parse_block_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_BLOCK_STMT, line);
    node->block.statements = NULL;
    StatementList *tail = NULL;
    ASTNode *statement = NULL;

    eat(parser, TOKEN_LEFT_BRACE);  // 消耗"{"
    enter_scope(parser->env);
    while (parser->current_token->type != TOKEN_RIGHT_BRACE) {
        if (parser->current_token->type == TOKEN_EOF) {
            SYNTAX_ERROR_EXIT(parser, "右花括号", TOKEN_RIGHT_BRACE);
        }
        
        statement = parse_statement(parser);
        
        if (!node->block.statements) {
            node->block.statements = new_statement_list(statement);
            tail = node->block.statements;
        } else {
            tail->next = new_statement_list(statement);
            tail = tail->next;
        }
    }
    
    eat(parser, TOKEN_RIGHT_BRACE);  // 消耗"}"
    exit_scope(parser->env);
    return node;
}

/* 解析导入语句 */
ASTNode* parse_import_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_IMPORT_STMT, line);
    
    eat(parser, TOKEN_IMPORT);  // 消耗"导入"
    
    if (parser->current_token->type != TOKEN_STRING) {
        SYNTAX_ERROR_EXIT(parser, "字符串", TOKEN_STRING);
    }
    
    node->import.module = strdup(parser->current_token->value);
    eat(parser, TOKEN_STRING);
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    return node;
}

/* 解析导出语句 */
ASTNode* parse_export_statement(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *node = new_ast_node(AST_EXPORT_STMT, line);
    
    eat(parser, TOKEN_EXPORT);  // 消耗"导出"
    
    if (parser->current_token->type != TOKEN_IDENTIFIER) {
        SYNTAX_ERROR_EXIT(parser, "标识符", TOKEN_IDENTIFIER);
    }
    
    node->export.name = strdup(parser->current_token->value);
    eat(parser, TOKEN_IDENTIFIER);
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    return node;
}

/* 解析表达式 */
ASTNode* parse_expression(Parser *parser) {
    return parse_assignment_expression(parser);
}

/* 解析赋值表达式 */
ASTNode* parse_assignment_expression(Parser *parser) {
    int line = parser->current_token->line;
    
    ASTNode *left = parse_logical_or_expression(parser);
    TokenType tokenType = parser->current_token->type;

    if (tokenType == TOKEN_ASSIGN ||
        tokenType == TOKEN_ADD_ASSIGN ||
        tokenType == TOKEN_SUB_ASSIGN ||
        tokenType == TOKEN_MUL_ASSIGN ||
        tokenType == TOKEN_DIV_ASSIGN ||
        tokenType == TOKEN_MOD_ASSIGN) {
        eat(parser, tokenType);
        
        ASTNode *right = parse_assignment_expression(parser);
        
        ASTNode *node = new_ast_node(AST_ASSIGNMENT_EXPR, line);
        node->assignment.operator = token_to_operator(tokenType);
        node->assignment.left = left;
        node->assignment.right = right;
        node->inferred_type = infer_type(parser->env, node);
        return node;
    }
    left->inferred_type = infer_type(parser->env, left);
    return left;
}

/* 解析逻辑或表达式 */
ASTNode* parse_logical_or_expression(Parser *parser) {
    int line = parser->current_token->line;
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;

    ASTNode *node = parse_logical_and_expression(parser);
    
    while (parser->current_token->type == TOKEN_OR) {
        eat(parser, TOKEN_OR);

        right = parse_logical_and_expression(parser);
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(TOKEN_OR);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析逻辑与表达式 */
ASTNode* parse_logical_and_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;

    ASTNode *node = parse_equality_expression(parser);
    
    while (parser->current_token->type == TOKEN_AND) {
        eat(parser, TOKEN_AND);
        
        right = parse_equality_expression(parser);
        
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(TOKEN_AND);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析相等表达式 */
ASTNode* parse_equality_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;
    
    ASTNode *node = parse_comparison_expression(parser);

    TokenType tokenType = parser->current_token->type;
    while (tokenType == TOKEN_EQ || tokenType == TOKEN_NE) {
        eat(parser, tokenType);
        
        right = parse_comparison_expression(parser);
        
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(tokenType);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
        tokenType = parser->current_token->type;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析比较表达式 */
ASTNode* parse_comparison_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;
    
    ASTNode *node = parse_additive_expression(parser);

    TokenType tokenType = parser->current_token->type;
    while (tokenType == TOKEN_GT || tokenType == TOKEN_LT ||
           tokenType == TOKEN_GE || tokenType == TOKEN_LE ) {
        
        eat(parser, tokenType);
        
        right = parse_additive_expression(parser);
        
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(tokenType);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
        tokenType = parser->current_token->type;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析加减表达式 */
ASTNode* parse_additive_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;
    
    ASTNode *node = parse_multiplicative_expression(parser);

    TokenType tokenType = parser->current_token->type;
    while (tokenType == TOKEN_PLUS || tokenType == TOKEN_MINUS) {
        eat(parser, tokenType);
        
        right = parse_multiplicative_expression(parser);
        
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(tokenType);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
        tokenType = parser->current_token->type;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析乘除表达式 */
ASTNode* parse_multiplicative_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;
    ASTNode *node = parse_exponential_expression(parser);

    TokenType tokenType = parser->current_token->type;
    while (tokenType == TOKEN_STAR || tokenType == TOKEN_SLASH ||
           tokenType == TOKEN_PERCENT) {
        eat(parser, tokenType);
        
        right = parse_exponential_expression(parser);
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(tokenType);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
        tokenType = parser->current_token->type; 
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析指数表达式 */
ASTNode* parse_exponential_expression(Parser *parser) {
    ASTNode *right = NULL;
    ASTNode *new_node = NULL;
    int line = parser->current_token->line;
    
    ASTNode *node = parse_unary_expression(parser);
    
    while (parser->current_token->type == TOKEN_POW) {
        eat(parser, TOKEN_POW);
        
        right = parse_unary_expression(parser);
        
        new_node = new_ast_node(AST_BINARY_EXPR, line);
        new_node->binary_expr.operator = token_to_operator(TOKEN_POW);
        new_node->binary_expr.left = node;
        new_node->binary_expr.right = right;
        
        node = new_node;
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析一元表达式 */
ASTNode* parse_unary_expression(Parser *parser) {
    int line = parser->current_token->line;
    TokenType tokenType = parser->current_token->type;

    if (tokenType == TOKEN_PLUS || tokenType == TOKEN_MINUS ||
        tokenType == TOKEN_NOT) {
        eat(parser, tokenType);
        
        ASTNode *operand = parse_unary_expression(parser);
        
        ASTNode *node = new_ast_node(AST_UNARY_EXPR, line);
        node->unary_expr.operator = token_to_operator(tokenType);
        node->unary_expr.operand = operand;
        node->inferred_type = infer_type(parser->env, node);
        return node;
    }
    
    return parse_primary_expression(parser);
}

int get_numeric_type(char *str) {
    if (!str) return LITERAL_UNKNOWN;

    char *endptr = NULL;
    long double value = strtold(str, &endptr);
    char *dot_ptr = strchr(str, '.');
    if (*endptr == '\0' && endptr != str)
    {
        if (dot_ptr == NULL)
        {
            if (IS_UINT_TYPE(value, UINT8_MAX)) return LITERAL_UINT8;
            if (IS_UINT_TYPE(value, UINT16_MAX)) return LITERAL_UINT16;
            if (IS_UINT_TYPE(value, UINT32_MAX)) return LITERAL_UINT32;
            if (IS_UINT_TYPE(value, UINT64_MAX)) return LITERAL_UINT64;
            if (IS_INT_TYPE(value, INT8_MIN, INT8_MAX)) return LITERAL_INT8;
            if (IS_INT_TYPE(value, INT16_MIN, INT16_MAX)) return LITERAL_INT16;
            if (IS_INT_TYPE(value, INT32_MIN, INT32_MAX)) return LITERAL_INT32;
            if (IS_INT_TYPE(value, INT64_MIN, INT64_MAX)) return LITERAL_INT64;
        }
        else
        {
            if (IS_FLOAT_TYPE(value, -65504.0, 65504.0)) return LITERAL_FLOAT16;
            if (IS_FLOAT_TYPE(value, -FLT_MAX, FLT_MAX)) return LITERAL_FLOAT32;
            if (IS_FLOAT_TYPE(value, -DBL_MAX, DBL_MAX)) return LITERAL_FLOAT64;
        }
    }

    return LITERAL_UNKNOWN;  // 不属于任何数值类型
}

/* 解析主表达式 */
ASTNode* parse_primary_expression(Parser *parser) {
    int line = parser->current_token->line;
    ASTNode *node = NULL;
    
    if (parser->current_token->type == TOKEN_NUMBER) {
        node = new_ast_node(AST_LITERAL_EXPR, line);
        node->literal.literal_type = get_numeric_type(parser->current_token->value);
        node->literal.value = strdup(parser->current_token->value);
        eat(parser, TOKEN_NUMBER);
        
    } else if (parser->current_token->type == TOKEN_STRING) {
        node = new_ast_node(AST_LITERAL_EXPR, line);
        node->literal.literal_type = LITERAL_STRING;
        node->literal.value = strdup(parser->current_token->value);
        eat(parser, TOKEN_STRING);
        
    } else if (parser->current_token->type == TOKEN_CHAR) {
        node = new_ast_node(AST_LITERAL_EXPR, line);
        node->literal.literal_type = LITERAL_CHAR;
        node->literal.value = strdup(parser->current_token->value);
        eat(parser, TOKEN_CHAR);
        
    } else if (parser->current_token->type == TOKEN_BOOLEAN) {
        node = new_ast_node(AST_LITERAL_EXPR, line);
        node->literal.literal_type = LITERAL_BOOLEAN;
        node->literal.value = strdup(parser->current_token->value);
        eat(parser, TOKEN_BOOLEAN);
        
    } else if (parser->current_token->type == TOKEN_NULL) {
        node = new_ast_node(AST_LITERAL_EXPR, line);
        node->literal.literal_type = LITERAL_NULL;
        node->literal.value = strdup(parser->current_token->value);
        eat(parser, TOKEN_NULL);
        
    } else if (parser->current_token->type == TOKEN_IDENTIFIER) {
        node = new_ast_node(AST_IDENTIFIER_EXPR, line);
        node->identifier.name = strdup(parser->current_token->value);
        eat(parser, TOKEN_IDENTIFIER);
        
    } else if (parser->current_token->type == TOKEN_LEFT_PAREN) {
        eat(parser, TOKEN_LEFT_PAREN);  // 消耗"("
        node = parse_expression(parser);
        
        if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
            SYNTAX_ERROR_EXIT(parser, "右括号", TOKEN_RIGHT_PAREN);
        }
        eat(parser, TOKEN_RIGHT_PAREN);  // 消耗")"
        
    } else {
        SYNTAX_ERROR_EXIT(parser, "括号完整", parser->current_token->type);
    }
    ASTNode *call_node = NULL;
    ASTNode *array_access_node = NULL;
    ASTNode *object_access_node = NULL;
    /* 处理函数调用、数组访问和对象访问 */
    while (1) {
        if (parser->current_token->type == TOKEN_LEFT_PAREN) {
            eat(parser, TOKEN_LEFT_PAREN);  // 消耗"("
            
            call_node = new_ast_node(AST_CALL_EXPR, line);
            call_node->call.callee = node; // 要找到函数的定义(todo)
            call_node->call.arguments = NULL;
            
            if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
                call_node->call.arguments = parse_expression_list(parser);
            }
            
            if (parser->current_token->type != TOKEN_RIGHT_PAREN) {
                SYNTAX_ERROR_EXIT(parser, "右括号", TOKEN_RIGHT_PAREN);
            }
            eat(parser, TOKEN_RIGHT_PAREN);  // 消耗")"
            
            node = call_node;
            
        } else if (parser->current_token->type == TOKEN_LEFT_BRACKET) {
            eat(parser, TOKEN_LEFT_BRACKET);  // 消耗"["
            
            array_access_node = new_ast_node(AST_ARRAY_ACCESS_EXPR, line);
            array_access_node->array_access.object = node;
            array_access_node->array_access.index = parse_expression(parser);
            
            if (parser->current_token->type != TOKEN_RIGHT_BRACKET) {
                SYNTAX_ERROR_EXIT(parser, "右方括号", TOKEN_RIGHT_BRACKET);
            }
            eat(parser, TOKEN_RIGHT_BRACKET);  // 消耗"]"
            
            node = array_access_node;
            
        } else if (parser->current_token->type == TOKEN_DOT) {
            eat(parser, TOKEN_DOT);  // 消耗"."
            
            if (parser->current_token->type != TOKEN_IDENTIFIER) {
                SYNTAX_ERROR_EXIT(parser, "标识符", TOKEN_IDENTIFIER);
            }
            
            object_access_node = new_ast_node(AST_OBJECT_ACCESS_EXPR, line);
            object_access_node->object_access.object = node;
            object_access_node->object_access.property = strdup(parser->current_token->value);
            eat(parser, TOKEN_IDENTIFIER);
            
            node = object_access_node;
            
        } else {
            break;
        }
    }
    node->inferred_type = infer_type(parser->env, node);
    return node;
}

/* 解析表达式列表 */
ExpressionList* parse_expression_list(Parser *parser) {
    ExpressionList *head = NULL;
    ExpressionList *tail = NULL;
    ASTNode *expression = NULL;
    ExpressionList *item = NULL;

    do {
        expression = parse_expression(parser);
        item = new_expression_list(expression);
        
        if (!head) {
            head = item;
            tail = item;
        } else {
            tail->next = item;
            tail = item;
        }
        
        if (parser->current_token->type != TOKEN_COMMA) {
            break;
        }
        eat(parser, TOKEN_COMMA);
    } while (1);
    
    return head;
}

/* 解析语句 */
ASTNode* parse_statement(Parser *parser) {
    TokenType tokenType = parser->current_token->type;

    if (tokenType == TOKEN_VAR || tokenType == TOKEN_CONST) {
        return parse_var_declaration(parser);
    } else if (tokenType == TOKEN_FUNC) {
        return parse_func_declaration(parser);
    } else if (tokenType == TOKEN_IF) {
        return parse_if_statement(parser);
    } else if (tokenType == TOKEN_FOR) {
        return parse_for_statement(parser);
    } else if (tokenType == TOKEN_WHILE) {
        return parse_while_statement(parser);
    } else if (tokenType == TOKEN_BREAK) {
        return parse_break_statement(parser);
    } else if (tokenType == TOKEN_CONTINUE) {
        return parse_continue_statement(parser);
    } else if (tokenType == TOKEN_RETURN) {
        return parse_return_statement(parser);
    } else if (tokenType == TOKEN_IMPORT) {
        return parse_import_statement(parser);
    } else if (tokenType == TOKEN_EXPORT) {
        return parse_export_statement(parser);
    } else if (tokenType == TOKEN_STRUCT || tokenType == TOKEN_UNION) {
        return parse_struct_or_union(parser);
    }
    else if (tokenType == TOKEN_LEFT_BRACE) {
        return parse_block_statement(parser);
    }
    
    // 表达式语句
    ASTNode *expr = parse_expression(parser);
    
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    ASTNode *node = new_ast_node(AST_EXPR_STMT, expr->line);
    node->expr_stmt.expression = expr;
    
    return node;
}

/* 解析成员声明 - 支持嵌套 */
ASTNode* parse_member_declaration(Parser* parser) {
    // 检查是否是嵌套的结构体/联合体声明
    if (parser->current_token->type == TOKEN_STRUCT ||
        parser->current_token->type == TOKEN_UNION) {
        return parse_struct_or_union(parser);
    }

    // 普通成员声明
    char* name = NULL;
    
    // 解析名称
    if (parser->current_token->type != TOKEN_IDENTIFIER) {
        SYNTAX_ERROR_EXIT(parser, "标识符", TOKEN_IDENTIFIER);
    }
    
    name = strdup(parser->current_token->value);
    eat(parser, TOKEN_IDENTIFIER);
    
    // 创建成员节点
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->type = AST_MEMBER_DECL;
    node->line = parser->current_token->line;
    node->member_decl.name = name;
    node->inferred_type = TYPE_ANY;
    return node;
}

MemberList* parse_member_list(Parser* parser) {
    MemberList* head = NULL;
    MemberList* tail = NULL;
    ASTNode* member_node = NULL;
    MemberList* item = NULL;

    while (1) {
        // 检查是否结束
        if (parser->current_token->type == TOKEN_RIGHT_BRACE) {
            break;
        }
        
        // 解析成员声明
        member_node = parse_member_declaration(parser);
        if (!member_node) {
            free_member_list(head);
            return NULL;
        }
        
        // 创建MemberList节点
        item = (MemberList*)malloc(sizeof(MemberList));
        item->decl = member_node;
        item->next = NULL;
        
        if (!head) {
            head = tail = item;
        } else {
            tail->next = item;
            tail = item;
        }
        
        // 仅普通成员需要分号
        if (member_node->type == AST_MEMBER_DECL) {
            if (parser->current_token->type != TOKEN_SEMICOLON) {
                free_member_list(head);
                SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
            }
            eat(parser, TOKEN_SEMICOLON);
        }
    }
    
    return head;
}

/* 解析结构体或联合体声明 - 支持匿名嵌套 */
ASTNode* parse_struct_or_union(Parser* parser) {
    ASTNodeType decl_type = (parser->current_token->type == TOKEN_STRUCT) ? 
                           AST_STRUCT_DECL : AST_UNION_DECL;
    
    eat(parser, parser->current_token->type);  // 消耗'组'或'团'
    
    // 解析名称
    if (parser->current_token->type != TOKEN_IDENTIFIER) {
        SYNTAX_ERROR_EXIT(parser, "结构体/联合体名称", TOKEN_IDENTIFIER);
    }
    
    char* name = strdup(parser->current_token->value);
    eat(parser, TOKEN_IDENTIFIER);
    
    // 检查并消耗'{'
    if (parser->current_token->type != TOKEN_LEFT_BRACE) {
        free(name);
        SYNTAX_ERROR_EXIT(parser, "左花括号", TOKEN_LEFT_BRACE);
    }
    eat(parser, TOKEN_LEFT_BRACE);

    // 解析成员列表
    MemberList* members = parse_member_list(parser);
    if (!members) {
        free(name);
        return NULL;
    }
    
    // 检查并消耗'}'
    if (parser->current_token->type != TOKEN_RIGHT_BRACE) {
        free(name);
        SYNTAX_ERROR_EXIT(parser, "右花括号", TOKEN_RIGHT_BRACE);
    }
    eat(parser, TOKEN_RIGHT_BRACE);

    // 检查并消耗';'
    if (parser->current_token->type != TOKEN_SEMICOLON) {
        free_member_list(members);
        SYNTAX_ERROR_EXIT(parser, "分号", TOKEN_SEMICOLON);
    }
    eat(parser, TOKEN_SEMICOLON);
    
    // 创建AST节点
    ASTNode* node = new_ast_node(decl_type, parser->current_token->line);
    node->inferred_type = TYPE_ANY;
    node->struct_or_union_decl.name = name;
    node->struct_or_union_decl.members = members;
    
    return node;
}