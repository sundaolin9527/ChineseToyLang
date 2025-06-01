#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include "../../src/frontend/lexer/lexer.h"
#include "../../src/utils/util.h"

/* 测试词法分析器 */
int main(int argc, char* argv[]) {
    // 设置本地化环境以支持中文
    setlocale(LC_ALL, "");

    // 检查命令行参数
    if (argc != 2) {
        fprintf(stderr, "用法: %s <文件名>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // 从文件读取源代码
    char* source_code = read_file(argv[1]);
    if (!source_code) {
        return EXIT_FAILURE;
    }

    printf("正在分析文件: %s\n", argv[1]);
    printf("========================================\n");

    // 初始化词法分析器
    Lexer* lexer = init_lexer(source_code);
    if (!lexer) {
        fprintf(stderr, "无法初始化词法分析器\n");
        free(source_code);
        return EXIT_FAILURE;
    }

    printf("%-20s %-15s %-10s %s\n", "Token类型", "值", "行", "列");
    printf("----------------------------------------\n");

    // 逐个获取并打印token
    Token* token;
    while (1) {
        token = get_next_token(lexer);
        if (!token) {
            fprintf(stderr, "获取token失败\n");
            break;
        }

        // 打印token信息
        printf("%-20s %-15s %-10d %d\n", 
               token_type_to_string(token->type),
               token->value,
               token->line,
               token->column);

        // 遇到EOF时退出循环
        if (token->type == TOKEN_EOF) {
            free_token(token);
            break;
        }

        free_token(token);
    }

    // 释放资源
    free(source_code);
    free_lexer(lexer);

    return EXIT_SUCCESS;
}