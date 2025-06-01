#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>

typedef enum {
    TYPE_UNKNOWN,   // 未知类型
    TYPE_INT,       // 整型
    TYPE_FLOAT,     // 浮点型
    TYPE_BOOL,      // 布尔型
    TYPE_STRING,    // 字符串
    TYPE_VOID,      // 无类型
    TYPE_FUNCTION,  // 函数类型
    TYPE_ARRAY      // 数组类型
} TypeKind;

// 类型结构体
typedef struct Type {
    TypeKind kind;
    union {
        // 函数类型信息
        struct {
            struct Type* return_type;
            struct Type** param_types;
            int param_count;
        } func;
        
        // 数组类型信息
        struct {
            struct Type* element_type;
            int size; // -1表示动态数组
        } array;
    } meta;
} Type;

// 类型环境(符号表)
typedef struct TypeEnv {
    char* name;       // 变量/函数名
    Type* type;       // 类型信息
    struct TypeEnv* next; // 链表结构
} TypeEnv;

// 类型系统API
Type* new_type(TypeKind kind);
Type* new_function_type(Type* return_type, Type** params, int param_count);
Type* new_array_type(Type* element_type, int size);
void free_type(Type* type);

TypeEnv* new_type_env(const char* name, Type* type, TypeEnv* next);
Type* lookup_type(TypeEnv* env, const char* name);
TypeEnv* extend_type_env(TypeEnv* env, const char* name, Type* type);
void free_type_env(TypeEnv* env);

bool type_equal(Type* t1, Type* t2);
Type* type_join(Type* t1, Type* t2); // 类型联合(寻找共同类型)
const char* type_to_string(Type* type);

#endif // TYPE_H