
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include "Basic/Types.h"
#include "Basic/Utils.h"
#include "Frontend/Infer.h"

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
    
    free(source);
    
    return 0;
}

#ifdef __cplusplus
}
#endif