#ifndef UTILS_H
#define UTILS_H
#include <stdbool.h>
#include <stdint.h>
#include <float.h>
#include <math.h>
#include <locale.h>

char* read_file(const char* filename);

// 有符号整数范围
#define IS_INT_TYPE(value, min, max) \
    ((value) >= (min) && (value) <= (max) && floor(value) == (value))

// 无符号整数
#define IS_UINT_TYPE(value, max) \
    ((value) >= 0 && (value) <= (max) && floor(value) == (value))

// 浮点数范围
#define IS_FLOAT_TYPE(value, min, max) \
    ((value) >= (min) && (value) <= (max))
#endif /* UTILS_H */ 