
#include <stdio.h>
#include <stdlib.h>
#include "Basic/Utils.h"

// 从文件读取内容的函数
char* read_file(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("无法打开文件");
        return NULL;
    }

    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // 分配内存并读取内容
    char* content = (char*)malloc(file_size + 1);
    if (!content) {
        fclose(file);
        fprintf(stderr, "内存分配失败\n");
        return NULL;
    }

    size_t bytes_read = fread(content, 1, file_size, file);
    if (bytes_read != file_size) {
        fclose(file);
        free(content);
        fprintf(stderr, "读取文件失败\n");
        return NULL;
    }

    content[file_size] = '\0'; // 确保字符串以null结尾
    fclose(file);
    return content;
}