#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
// 包含词法分析器和语法分析器的头文件
#include "../../src/frontend/lexer/lexer.h"
#include "../../src/frontend/parser/parser.h"
#include "../../src/utils/util.h"

int main(int argc, char *argv[]) {
    // 设置本地化环境以支持中文
    setlocale(LC_ALL, "");
    
    // 检查命令行参数
    if (argc != 2) {
        fprintf(stderr, "用法: %s <源代码文件>\n", argv[0]);
        return 1;
    }
    
    char* source = read_file(argv[1]);
    if (!source) {
        return EXIT_FAILURE;
    }

    printf("正在分析文件: %s\n", argv[1]);
    printf("========================================\n");
    
    printf("从文件 '%s' 读取源代码...\n", argv[1]);
    printf("%s", source);

    // 创建词法分析器
    Lexer *lexer = init_lexer(source);
    
    // 创建语法分析器
    Parser *parser = init_parser(lexer);
    
    // 解析程序
    ASTNode *program = parse_program(parser);
    
    // 打印AST
    printf("\n生成的抽象语法树:\n");
    print_ast(program, 0);
    
    // 释放内存
    free_ast_node(program);
    free(parser);
    free_lexer(lexer);
    free(source);
    
    return 0;
}