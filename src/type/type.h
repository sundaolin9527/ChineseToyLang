#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include "../utils/utils.h"

typedef enum Type{
    TYPE_UNKNOWN,   // 未知类型
    TYPE_INT8,      // 1字节整型
    TYPE_INT16,     // 2字节整型
    TYPE_INT32,     // 4字节整型
    TYPE_INT64,     // 8字节整型
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
    TYPE_ANY        // 任意类型
} Type;

// 符号
typedef struct Symbol Symbol;
struct Symbol {
    Node symbolNode;
    char *symbolStr; // 符号名
    Type type;       // 符号类型
};

// 作用域
typedef struct Scope Scope;
struct Scope {
    Node scopeNode;
    Symbol *symbols; // 该作用域的符号表
};

// 类型环境
typedef struct TypeEnv TypeEnv;
struct TypeEnv {
    Scope *currScope;  // 当前作用域栈顶
    Scope *idle_scope; // 空闲作用域链表
    Symbol *idle_symbol; // 空闲符号链表
};

TypeEnv *initTypeEnv();
void freeTypeEnv(TypeEnv **typeEnv) ;
void enterScope(TypeEnv *env);
void exitScope(TypeEnv *env);
bool addSymbolToScope(TypeEnv *env, const char *name, Type type);
Symbol* findSymbolInScope(TypeEnv *env, const char *name);
void printCurrScopeSymbols(TypeEnv *env);
#endif // TYPE_H