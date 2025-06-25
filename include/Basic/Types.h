#ifndef TYPE_H
#define TYPE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdbool.h>
#include "Basic/LinkedList.h"

typedef enum TypeKind{
    TYPE_UNKNOWN = 0,   // 未知类型
    TYPE_INT8,      // 1字节有符号整型
    TYPE_INT16,     // 2字节有符号整型
    TYPE_INT32,     // 4字节有符号整型
    TYPE_INT64,     // 8字节有符号整型
    TYPE_UINT8,      // 1字节无符号整型
    TYPE_UINT16,     // 2字节无符号整型
    TYPE_UINT32,     // 4字节无符号整型
    TYPE_UINT64,     // 8字节无符号整型
    TYPE_FLOAT16,   // 2字节浮点型
    TYPE_FLOAT32,   // 4字节浮点型
    TYPE_FLOAT64,   // 8字节浮点型
    TYPE_BOOLEAN,   // 布尔型
    TYPE_STRING,    // 字符串
    TYPE_CHAR,      // 字符型
    TYPE_VOID,      // 无类型
    TYPE_FUNCTION,  // 函数类型
    TYPE_ARRAY,     // 数组类型
    TYPE_STRUCT,    // 结构体
    TYPE_UNION,     // 联合体
    TYPE_PTR,       // 指针类型
    TYPE_ANY,        // 任意类型
} TypeKind;

// 符号
#define SYMBOL_SIZE (192)  //最长64个汉字，每个汉字3字节，共192字节
typedef struct Symbol Symbol;
struct Symbol {
    Node symbolNode;
    char symbolStr[SYMBOL_SIZE+1]; // 符号名
    TypeKind type;       // 符号类型
};

// 作用域
typedef struct Scope Scope;
struct Scope {
    Node scopeNode;
    Node symbols; // 该作用域的所有符号表, 存放Symbol
};

// 类型环境
typedef struct TypeEnv TypeEnv;
struct TypeEnv {
    Node scopeHead;  // 作用域栈，存放Scope, 当前作用域是链尾
    Node idle_scope; // 空闲作用域链表，存放Scope
    Node idle_symbol; // 空闲符号链表，存放Symbol
};

TypeEnv *init_type_env(void);
void free_type_env(TypeEnv **typeEnv) ;
void enter_scope(TypeEnv *env);
void exit_scope(TypeEnv *env);
bool add_symbol_to_scope(TypeEnv *env, char *name, TypeKind type);
Symbol* find_symbol_in_scope(TypeEnv *env, char *name);
void print_currscope_symbols(TypeEnv *env);
const char* type_to_string(TypeKind type);

#ifdef __cplusplus
}
#endif
#endif // TYPE_H
