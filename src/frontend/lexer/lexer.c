#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <locale.h>
#include "lexer.h"

#define LEXER_ERROR_EXIT(fmt, ...) \
    do { \
        fprintf(stderr, "[词法分析错误] " fmt " (%s:%d)\n", ##__VA_ARGS__, __FILE__, __LINE__); \
        exit(EXIT_FAILURE); \
    } while (0)

/* 获取标识符或关键字对应的token*/
TokenType get_identifier_token(char *value){
    if (!value) return TOKEN_INIT;

    if (strncmp(value, "令", strlen("令") + 1) == 0) {
        return TOKEN_VAR;
    }
    else if (strncmp(value, "恒", strlen("恒") + 1) == 0) {
        return TOKEN_CONST;
    }
        if (strncmp(value, "函", strlen("函") + 1) == 0) {
        return TOKEN_FUNC;
    }
    else if (strncmp(value, "返", strlen("返") + 1) == 0) {
        return TOKEN_RETURN;
    }
    else if (strncmp(value, "若", strlen("若") + 1) == 0) {
        return TOKEN_IF;
    }
    else if (strncmp(value, "亦", strlen("亦") + 1) == 0) {
        return TOKEN_ELIF;
    }
        if (strncmp(value, "则", strlen("则") + 1) == 0) {
        return TOKEN_ELSE;
    }
    else if (strncmp(value, "遍", strlen("遍") + 1) == 0) {
        return TOKEN_FOR;
    }
        if (strncmp(value, "当", strlen("当") + 1) == 0) {
        return TOKEN_WHILE;
    }
    else if (strncmp(value, "撤", strlen("撤") + 1) == 0) {
        return TOKEN_BREAK;
    }
        if (strncmp(value, "过", strlen("过") + 1) == 0) {
        return TOKEN_CONTINUE;
    }
    else if (strncmp(value, "导入", strlen("导入") + 1) == 0) {
        return TOKEN_IMPORT;
    }
        if (strncmp(value, "导出", strlen("导出") + 1) == 0) {
        return TOKEN_EXPORT;
    }
    else if (strncmp(value, "组", strlen("组") + 1) == 0) {
        return TOKEN_STRUCT;
    }
    else if (strncmp(value, "团", strlen("团") + 1) == 0) {
        return TOKEN_UNION;
    }
    else if ((strncmp(value, "真", strlen("真") + 1) == 0) ||
             (strncmp(value, "假", strlen("假") + 1) == 0)) 
    {
        return TOKEN_BOOLEAN;
    }
    else if (strncmp(value, "空", strlen("空") + 1) == 0) {
        return TOKEN_NULL;
    }
    else
    {
        return TOKEN_IDENTIFIER;
    }
}

/**获取运算符token */
TokenType get_operator_token(char *value){
    if (!value) return TOKEN_INIT;

    if (strncmp(value, "+", strlen("+") + 1) == 0) {
        return TOKEN_PLUS;
    }
    else if (strncmp(value, "-", strlen("-") + 1) == 0) {
        return TOKEN_MINUS;
    }
    else if (strncmp(value, "*", strlen("*") + 1) == 0) {
        return TOKEN_STAR;
    }
    else if (strncmp(value, "/", strlen("/") + 1) == 0) {
        return TOKEN_SLASH;
    }
    else if (strncmp(value, "%", strlen("%") + 1) == 0) {
        return TOKEN_PERCENT;
    }
    else if (strncmp(value, "**", strlen("**") + 1) == 0) {
        return TOKEN_POW;
    }
    else if (strncmp(value, "==", strlen("==") + 1) == 0) {
        return TOKEN_EQ;
    }
    else if (strncmp(value, "!=", strlen("!=") + 1) == 0 ||
             strncmp(value, "！=", strlen("！=") + 1) == 0) {
        return TOKEN_NE;
    }
    else if (strncmp(value, ">", strlen(">") + 1) == 0) {
        return TOKEN_GT;
    }
    else if (strncmp(value, "<", strlen("<") + 1) == 0) {
        return TOKEN_LT;
    }
        if (strncmp(value, ">=", strlen(">=") + 1) == 0) {
        return TOKEN_GE;
    }
    else if (strncmp(value, "<=", strlen("<=") + 1) == 0) {
        return TOKEN_LE;
    }
    else if (strncmp(value, "&&", strlen("&&") + 1) == 0) {
        return TOKEN_AND;
    }
    else if (strncmp(value, "||", strlen("||") + 1) == 0) {
        return TOKEN_OR;
    }
    else if (strncmp(value, "!", strlen("!") + 1) == 0 ||
             strncmp(value, "！", strlen("！") + 1) == 0) {
        return TOKEN_NOT;
    }
    else if (strncmp(value, "=", strlen("=") + 1) == 0)
    {
        return TOKEN_ASSIGN;
    }
    else if (strncmp(value, "+=", strlen("+=") + 1) == 0) {
        return TOKEN_ADD_ASSIGN;
    }
    else if (strncmp(value, "-=", strlen("-=") + 1) == 0) {
        return TOKEN_SUB_ASSIGN;
    }
    else if (strncmp(value, "*=", strlen("/=") + 1) == 0) {
        return TOKEN_MUL_ASSIGN;
    }
    else if (strncmp(value, "/=", strlen("/=") + 1) == 0) {
        return TOKEN_DIV_ASSIGN;
    }
    else if (strncmp(value, "%=", strlen("%=") + 1) == 0) {
        return TOKEN_MOD_ASSIGN;
    }
    else
    {
        return TOKEN_MAX;
    }
}

/* 获取分隔符token */
TokenType get_separator_token(char *value){
    if (!value) return TOKEN_INIT;

    if (strncmp(value, "(", strlen("(") + 1) == 0 ||
        strncmp(value, "（", strlen("（") + 1) == 0) {
        return TOKEN_LEFT_PAREN;
    }
    else if (strncmp(value, ")", strlen(")") + 1) == 0 ||
             strncmp(value, "）", strlen("）") + 1) == 0) {
        return TOKEN_RIGHT_PAREN;
    }
    else if (strncmp(value, "[", strlen("[") + 1) == 0) {
        return TOKEN_LEFT_BRACKET;
    }
    else if (strncmp(value, "]", strlen("]") + 1) == 0) {
        return TOKEN_RIGHT_BRACKET;
    }
    else if (strncmp(value, "{", strlen("{") + 1) == 0) {
        return TOKEN_LEFT_BRACE;
    }
    else if (strncmp(value, "}", strlen("}") + 1) == 0) {
        return TOKEN_RIGHT_BRACE;
    }
    else if (strncmp(value, ",", strlen(",") + 1) == 0 ||
             strncmp(value, "，", strlen("，") + 1) == 0) {
        return TOKEN_COMMA;
    }
    else if (strncmp(value, ";", strlen(";") + 1) == 0 ||
             strncmp(value, "；", strlen("；") + 1) == 0) {
        return TOKEN_SEMICOLON;
    }
    else if (strncmp(value, ":", strlen(":") + 1) == 0 ||
             strncmp(value, "：", strlen("：") + 1) == 0) {
        return TOKEN_COLON;
    }
    else if (strncmp(value, "=>", strlen("=>") + 1) == 0) {
        return TOKEN_ARROW;
    }
    else if (strncmp(value, "...", strlen("...") + 1) == 0) {
        return TOKEN_SPREAD;
    }
    else if (strncmp(value, ".", strlen(".") + 1) == 0) {
        return TOKEN_DOT;
    }
    return TOKEN_MAX;
}

/* 初始化词法分析器 */
Lexer* init_lexer(const char *input) {
    Lexer *lexer = (Lexer*)malloc(sizeof(Lexer));
    if (!lexer) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    lexer->input = input;
    lexer->position = 0;
    lexer->line = 1;
    lexer->column = 1;
    return lexer;
}

/* 获取下一个字符 */
char peek(Lexer *lexer) {
    if (!lexer || !lexer->input) return '\0';
    return lexer->input[lexer->position];
}

/* 读取下一个字符 */
char advance(Lexer *lexer) {
    if (!lexer || !lexer->input || peek(lexer) == '\0') return '\0';
    
    char current = lexer->input[lexer->position];
    
    if (current == '\n') {
        lexer->line++;
        lexer->column = 1;
    } else {
        lexer->column++;
    }
    
    lexer->position++;
    return current;
}

/* 判断是否是空白字符 */
int is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

/* 判断是否是字母 */
int is_letter(char c) {
    return isalpha((unsigned char)c) || c == '_';
}

/* 判断是否是数字 */
int is_digit(char c) {
    return isdigit((unsigned char)c);
}

/* 判断是否是中文字符 */
int is_chinese(wchar_t wc) {
    return (wc >= 0x4E00 && wc <= 0x9FFF);
}

/* 跳过空白字符 */
void skip_whitespace(Lexer *lexer) {
    if (!lexer) return;
    while (is_whitespace(peek(lexer))) {
        advance(lexer);
    }
}

/* 创建新的词法单元 */
Token* new_token(TokenType type, const char *value, int line, int column) {
    if (!value) return NULL;
    
    Token *token = (Token*)malloc(sizeof(Token));
    if (!token) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    token->type = type;
    token->value = strdup(value);
    if (!token->value) {
        free(token);
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    token->line = line;
    token->column = column;
    return token;
}

/* 解析标识符或关键字 */
Token* parse_identifier(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position;
    int line = lexer->line;
    int column = lexer->column;
    
    mbstate_t state = {0};
    size_t length = 0;
    wchar_t wc;
    
    // 计算标识符长度（以字节为单位）
    while (1) {
        if (lexer->position + length >= strlen(lexer->input)) break;
        
        int bytes = mbrtowc(&wc, lexer->input + lexer->position + length, MB_CUR_MAX, &state);
        
        if (bytes <= 0) {
            break;
        }
        
        if (is_chinese(wc) || is_letter(lexer->input[lexer->position + length]) || 
            is_digit(lexer->input[lexer->position + length])) {
            length += bytes;
        } else {
            break;
        }
    }
    
    // 处理空标识符的情况
    if (length == 0) {
        return new_token(TOKEN_MAX, "非法标识符", line, column);
    }
    
    // 分配内存并复制标识符
    char *value = (char*)malloc(length + 1);
    if (!value) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    memcpy(value, lexer->input + start, length);
    value[length] = '\0';
    
    // 更新词法分析器位置
    lexer->position += length;
    lexer->column += length;
    
    Token *token = NULL;
    TokenType tokenType = get_identifier_token(value);
    if (tokenType == TOKEN_MAX) {
        return new_token(TOKEN_MAX, "非法标识符", line, column);
    }
    token = new_token(tokenType, value, line, column);
    free(value);
    return token;
}

/* 解析数字 */
Token* parse_number(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position;
    int line = lexer->line;
    int column = lexer->column;
    int has_dot = 0;
    
    while (is_digit(peek(lexer)) || peek(lexer) == '.') {
        if (peek(lexer) == '.') {
            if (has_dot) break;
            has_dot = 1;
        }
        advance(lexer);
    }
    
    // 检查是否以点结尾（非法数字）
    if (lexer->input[lexer->position - 1] == '.') {
        lexer->position--; // 回退一个字符
        lexer->column--;
    }
    
    int length = lexer->position - start;
    
    // 处理空数字的情况
    if (length == 0) {
        return new_token(TOKEN_MAX, "非法数字", line, column);
    }
    
    char *value = (char*)malloc(length + 1);
    if (!value) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    memcpy(value, lexer->input + start, length);
    value[length] = '\0';
    
    Token *token = new_token(TOKEN_NUMBER, value, line, column);
    free(value);
    return token;
}

/* 解析字符串 */
Token* parse_string(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position + 1; // 跳过开始的引号
    int line = lexer->line;
    int column = lexer->column + 1;
    
    advance(lexer); // 跳过开始的引号
    
    int escaped = 0;
    while (peek(lexer) != '\0') {
        if (peek(lexer) == '"' && !escaped) {
            break;
        }
        
        if (peek(lexer) == '\\') {
            escaped = !escaped;
        } else {
            escaped = 0;
        }
        
        advance(lexer);
    }
    
    if (peek(lexer) == '\0') {
        return new_token(TOKEN_MAX, "未闭合的字符串", line, column);
    }
    
    int length = lexer->position - start;
    char *value = (char*)malloc(length + 1);
    if (!value) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    memcpy(value, lexer->input + start, length);
    value[length] = '\0';
    
    advance(lexer); // 跳过结束的引号
    
    Token *token = new_token(TOKEN_STRING, value, line, column);
    free(value);
    return token;
}

/* 解析字符 */
Token* parse_char(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position + 1; // 跳过开始的撇号
    int line = lexer->line;
    int column = lexer->column + 1;
    
    advance(lexer); // 跳过开始的撇号
    
    if (peek(lexer) == '\0') {
        return new_token(TOKEN_MAX, "未闭合的字符", line, column);
    }
    
    if (peek(lexer) == '\\') {
        advance(lexer); // 跳过转义字符
    }
    
    if (peek(lexer) == '\'') {
        return new_token(TOKEN_MAX, "空字符", line, column);
    }
    
    advance(lexer); // 读取字符
    
    if (peek(lexer) != '\'') {
        return new_token(TOKEN_MAX, "字符太长", line, column);
    }
    
    advance(lexer); // 跳过结束的撇号
    
    int length = lexer->position - start - 2; // 减去开始和结束的撇号
    
    // 处理空字符的情况
    if (length <= 0) {
        return new_token(TOKEN_MAX, "非法字符", line, column);
    }
    
    char *value = (char*)malloc(length + 1);
    if (!value) {
        LEXER_ERROR_EXIT("内存分配失败\n");
    }
    
    memcpy(value, lexer->input + start, length);
    value[length] = '\0';
    
    Token *token = new_token(TOKEN_CHAR, value, line, column);
    free(value);
    return token;
}

/* 解析运算符 */
Token* parse_operator(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position;
    int line = lexer->line;
    int column = lexer->column;
    TokenType tokenType = TOKEN_INIT;
    char *value = NULL;

    // 尝试匹配最长的运算符
    for (int len = 3; len > 0; len--) {
        if (start + len > strlen(lexer->input)) continue;
        
        value = (char*)malloc(len + 1);
        if (!value) {
            LEXER_ERROR_EXIT("内存分配失败\n");
        }
        
        memcpy(value, lexer->input + start, len);
        value[len] = '\0';
        
        if ((tokenType = get_operator_token(value)) != TOKEN_MAX) {
            lexer->position += len;
            lexer->column += len;
            
            Token *token = new_token(tokenType, value, line, column);
            free(value);
            return token;
        }
        
        free(value);
    }
    
    // 如果没有匹配到任何运算符，返回错误
    char error[20];
    sprintf(error, "未知运算符: %c", peek(lexer));
    advance(lexer);
    
    return new_token(TOKEN_MAX, error, line, column);
}

/* 解析分隔符 */
Token* parse_separator(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    
    int start = lexer->position;
    int line = lexer->line;
    int column = lexer->column;
    TokenType tokenType = TOKEN_INIT;
    char *value = NULL;

    // 尝试匹配最长的分隔符
    for (int len = 3; len > 0; len--) {
        if (start + len > strlen(lexer->input)) continue;
        
        value = (char*)malloc(len + 1);
        if (!value) {
            LEXER_ERROR_EXIT("内存分配失败\n");
        }
        
        memcpy(value, lexer->input + start, len);
        value[len] = '\0';
        
        if ((tokenType = get_separator_token(value)) != TOKEN_MAX) {
            lexer->position += len;
            lexer->column += len;
            
            Token *token = new_token(tokenType, value, line, column);
            free(value);
            return token;
        }
        
        free(value);
    }
    
    // 如果没有匹配到任何分隔符，返回错误
    char error[20];
    sprintf(error, "未知分隔符: %c", peek(lexer));
    advance(lexer);
    
    return new_token(TOKEN_MAX, error, line, column);
}

/* 获取下一个词法单元 */
Token* get_next_token(Lexer *lexer) {
    if (!lexer || !lexer->input) return NULL;
    char *value = NULL;
    // 检查是否是中文字符或字母开头的标识符
    mbstate_t state = {0};
    wchar_t wc = {'\0'};
    int bytes = 0;
    while (peek(lexer) != '\0') {
        if (is_whitespace(peek(lexer))) {
            skip_whitespace(lexer);
            continue;
        }
        
        bytes = mbrtowc(&wc, lexer->input + lexer->position, MB_CUR_MAX, &state);
        
        if (bytes > 0 && (is_chinese(wc) || is_letter(peek(lexer)))) {
            return parse_identifier(lexer);
        }
        
        if (is_digit(peek(lexer))) {
            return parse_number(lexer);
        }
        
        if (peek(lexer) == '"') {
            return parse_string(lexer);
        }
        
        if (peek(lexer) == '\'') {
            return parse_char(lexer);
        }
        
        // 检查是否是运算符或分隔符
        for (int len = 3; len > 0; len--) {
            if (lexer->position + len > strlen(lexer->input)) continue;
            
            value = (char*)malloc(len + 1);
            if (!value) {
                LEXER_ERROR_EXIT("内存分配失败\n");
            }
            
            memcpy(value, lexer->input + lexer->position, len);
            value[len] = '\0';
            
            if (get_operator_token(value) != TOKEN_MAX) {
                free(value);
                return parse_operator(lexer);
            }
            
            if (get_separator_token(value) != TOKEN_MAX) {
                free(value);
                return parse_separator(lexer);
            }
            
            free(value);
        }
        
        // 如果都不匹配，返回错误
        char error[20];
        sprintf(error, "非法字符: %c", peek(lexer));
        Token *token = new_token(TOKEN_MAX, error, lexer->line, lexer->column);
        advance(lexer);
        return token;
    }
    
    // 到达输入末尾
    return new_token(TOKEN_EOF, "", lexer->line, lexer->column);
}

/* 释放词法单元的内存 */
void free_token(Token *token) {
    if (!token) return;
    if (token->value) free(token->value);
    free(token);
}

/* 释放词法分析器的内存 */
void free_lexer(Lexer *lexer) {
    if (!lexer) return;
    free(lexer);
}

const char* token_type_to_string(TokenType type) {
    switch (type) {
        case TOKEN_VAR: return "TOKEN_VAR (令)";
        case TOKEN_CONST: return "TOKEN_CONST (恒)";
        case TOKEN_FUNC: return "TOKEN_FUNC (函)";
        case TOKEN_RETURN: return "TOKEN_RETURN (返)";
        case TOKEN_IF: return "TOKEN_IF (若)";
        case TOKEN_ELSE: return "TOKEN_ELSE (则)";
        case TOKEN_FOR: return "TOKEN_FOR (遍)";
        case TOKEN_WHILE: return "TOKEN_WHILE (当)";
        case TOKEN_BREAK: return "TOKEN_BREAK (撤)";
        case TOKEN_CONTINUE: return "TOKEN_CONTINUE (过)";
        case TOKEN_IMPORT: return "TOKEN_IMPORT (导入)";
        case TOKEN_EXPORT: return "TOKEN_EXPORT (导出)";
        case TOKEN_STRUCT: return "TOKEN_STRUCT (组)";
        case TOKEN_UNION: return "TOKEN_UNION (团)";
        case TOKEN_BOOLEAN: return "TOKEN_BOOLEAN (真/假)";
        case TOKEN_NULL: return "TOKEN_NULL (空)";
        case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
        case TOKEN_NUMBER: return "TOKEN_NUMBER";
        case TOKEN_STRING: return "TOKEN_STRING";
        case TOKEN_CHAR: return "TOKEN_CHAR";
        case TOKEN_PLUS: return "TOKEN_PLUS (+)";
        case TOKEN_MINUS: return "TOKEN_MINUS (-)";
        case TOKEN_STAR: return "TOKEN_STAR (*)";
        case TOKEN_SLASH: return "TOKEN_SLASH (/)";
        case TOKEN_PERCENT: return "TOKEN_PERCENT (%)";
        case TOKEN_POW: return "TOKEN_POW (**)";
        case TOKEN_EQ: return "TOKEN_EQ (==)";
        case TOKEN_NE: return "TOKEN_NE (!=)";
        case TOKEN_GT: return "TOKEN_GT (>)";
        case TOKEN_LT: return "TOKEN_LT (<)";
        case TOKEN_GE: return "TOKEN_GE (>=)";
        case TOKEN_LE: return "TOKEN_LE (<=)";
        case TOKEN_AND: return "TOKEN_AND (&&)";
        case TOKEN_OR: return "TOKEN_OR (||)";
        case TOKEN_NOT: return "TOKEN_NOT (!)";
        case TOKEN_ASSIGN: return "TOKEN_ASSIGN (=)";
        case TOKEN_ADD_ASSIGN: return "TOKEN_ADD_ASSIGN (+=)";
        case TOKEN_SUB_ASSIGN: return "TOKEN_SUB_ASSIGN (-=)";
        case TOKEN_MUL_ASSIGN: return "TOKEN_MUL_ASSIGN (*=)";
        case TOKEN_DIV_ASSIGN: return "TOKEN_DIV_ASSIGN (/=)";
        case TOKEN_MOD_ASSIGN: return "TOKEN_MOD_ASSIGN (%=)";
        case TOKEN_LEFT_PAREN: return "TOKEN_LEFT_PAREN (()";
        case TOKEN_RIGHT_PAREN: return "TOKEN_RIGHT_PAREN ())";
        case TOKEN_LEFT_BRACKET: return "TOKEN_LEFT_BRACKET ([)";
        case TOKEN_RIGHT_BRACKET: return "TOKEN_RIGHT_BRACKET (])";
        case TOKEN_LEFT_BRACE: return "TOKEN_LEFT_BRACE ({)";
        case TOKEN_RIGHT_BRACE: return "TOKEN_RIGHT_BRACE (})";
        case TOKEN_COMMA: return "TOKEN_COMMA (,)";
        case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON (;)";
        case TOKEN_COLON: return "TOKEN_COLON (:)";
        case TOKEN_ARROW: return "TOKEN_ARROW (=>)";
        case TOKEN_SPREAD: return "TOKEN_SPREAD (...)";
        case TOKEN_DOT: return "TOKEN_DOT (.)";
        case TOKEN_EOF: return "TOKEN_EOF";
        case TOKEN_MAX: return "TOKEN_MAX (错误)";
        default: return "未知 Token 类型";
    }
}