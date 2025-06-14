#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "type.h"

// 清理类型环境
void cleanup_type_env(TypeEnv *env) {
    if (!env) return;

    LIST_FREE(&(env->scopeHead), Scope, scopeNode);
    LIST_FREE(&(env->idle_scope), Scope, scopeNode);
    LIST_FREE(&(env->idle_symbol), Symbol, symbolNode);
    return;
}

// 从空闲链表获取或创建新作用域
static Scope* get_or_create_scope(TypeEnv *env) {
    if (!env) return NULL;

    Node *node = LIST_POP_HEAD(&(env->idle_scope));
    Scope *currScope = (node == NULL) ? (Scope*)malloc(sizeof(Scope)) 
                       : LIST_ENTRY(node, Scope, scopeNode);
    
    if (currScope) {
        memset(currScope, 0, sizeof(Scope));
        INIT_LIST_HEAD(&(currScope->scopeNode));
        INIT_LIST_HEAD(&(currScope->symbols));
    }
    return currScope;
}

// 从空闲链表获取或创建新符号
static Symbol* get_or_create_symbol(TypeEnv *env) {
    if (!env) return NULL;

    Node *node = LIST_POP_HEAD(&(env->idle_symbol));
    Symbol *currSymbol = (node == NULL) ? (Symbol*)malloc(sizeof(Symbol)) 
                       : LIST_ENTRY(node, Symbol, symbolNode);
    
    if (currSymbol) {
        memset(currSymbol, 0, sizeof(Symbol));
        INIT_LIST_HEAD(&(currSymbol->symbolNode));
        currSymbol->type = TYPE_UNKNOWN;
    }
    return currSymbol;
}

// 初始化类型环境
TypeEnv *init_type_env() {
    TypeEnv *typeEnv = (TypeEnv*)malloc(sizeof(TypeEnv));
    if (typeEnv) {
        memset(typeEnv, 0, sizeof(TypeEnv));
        INIT_LIST_HEAD(&(typeEnv->scopeHead));
        INIT_LIST_HEAD(&(typeEnv->idle_scope));
        INIT_LIST_HEAD(&(typeEnv->idle_symbol));
    }
    return typeEnv;
}

void free_type_env(TypeEnv **typeEnv) {
    if (!typeEnv|| !(*typeEnv))
    {
        return;
    }

    cleanup_type_env(*typeEnv);
    free(*typeEnv);
    *typeEnv = NULL;
    return;
}

// 进入新作用域
void enter_scope(TypeEnv *env) {
    if (!env) return;
    
    Scope *newScope = get_or_create_scope(env);
    if (!newScope) return;

    // 将新作用域压入栈顶
    LIST_INSERT_TAIL(&(env->scopeHead) , &(newScope->scopeNode));
    return;
}

// 离开当前作用域
void exit_scope(TypeEnv *env) {
    if (!env || LIST_IS_EMPTY(&(env->scopeHead))) return;
    
    // 离开当前作用域
    Node *node = LIST_POP_TAIL(&(env->scopeHead));

    // 将符号移动到空闲链表
    Scope *currentScope = LIST_ENTRY(node, Scope, scopeNode);
    LIST_TRANSFER_ALL_TAIL(&(env->idle_symbol), &(currentScope->symbols));

    // 将作用域移动到空闲链表
    LIST_INSERT_TAIL(&(env->idle_scope), &(currentScope->scopeNode));
    
    return;
}

// 在当前作用域添加符号
bool add_symbol_to_scope(TypeEnv *env, const char *name, Type type) {
    if (!env || LIST_IS_EMPTY(&(env->scopeHead)) || !name) return false;
    
    // 检查符号是否已存在
    Node *scopeHead = LIST_PEEK_TAIL(&(env->scopeHead));
    Scope *currentScope = LIST_ENTRY(scopeHead, Scope, scopeNode);
    {
        Node *pos, *next;
        Symbol *existing = NULL;
        LIST_FOREACH_SAFE(&(currentScope->symbols), pos, next)
        {
            existing = LIST_ENTRY(pos, Symbol, symbolNode);
            if (strncmp(existing->symbolStr, name, strlen(name)+1) == 0 &&
                existing->type == type) {
                return true; // 符号已存在
            }
        }
    }

    // 创建新符号
    Symbol *symbol = get_or_create_symbol(env);
    if (!symbol) return false;
    strncpy(symbol->symbolStr, name, strlen(name)+1);
    symbol->type = type;

    // 添加到作用域符号表
    LIST_INSERT_TAIL(&(currentScope->symbols), &(symbol->symbolNode));
    
    return true;
}

// 查找符号（从当前作用域向外查找）
Symbol* find_symbol_in_scope(TypeEnv *env, const char *name) {
    if (!env || LIST_IS_EMPTY(&(env->scopeHead)) || !name) return NULL;
    
    Scope *currentScope = NULL;
    Symbol *currentSymbol = NULL;
    Node *posScope, *prevScope;
    Node *posSymbol, *prevSymbol;
    LIST_FOREACH_REVERSE_SAFE(&(env->scopeHead), posScope, prevScope)
    {
        currentScope = LIST_ENTRY(posScope, Scope, scopeNode);
        // 从最新的符号开始查找
        LIST_FOREACH_REVERSE_SAFE(&(currentScope->symbols), posSymbol, prevSymbol)
        {
            currentSymbol = LIST_ENTRY(posSymbol, Symbol, symbolNode);
            if (strncmp(currentSymbol->symbolStr, name, strlen(name) + 1) == 0) {
                return currentSymbol;
            }
        }
    }

    return NULL;
}

// 打印类型名称
const char* type_to_string(Type type) {
    switch (type) {
        case TYPE_UNKNOWN: return "UNKNOWN";
        case TYPE_INT8: return "INT8";
        case TYPE_INT16: return "INT16";
        case TYPE_INT32: return "INT32";
        case TYPE_INT64: return "INT64";
        case TYPE_UINT8: return "UINT8";
        case TYPE_UINT16: return "UINT16";
        case TYPE_UINT32: return "UINT32";
        case TYPE_UINT64: return "UINT64";
        case TYPE_FLOAT16: return "FLOAT16";
        case TYPE_FLOAT32: return "FLOAT32";
        case TYPE_FLOAT64: return "FLOAT64";
        case TYPE_BOOLEAN: return "BOOLEAN";
        case TYPE_STRING: return "STRING";
        case TYPE_CHAR: return "CHAR";
        case TYPE_VOID: return "VOID";
        case TYPE_FUNCTION: return "FUNCTION";
        case TYPE_ARRAY: return "ARRAY";
        case TYPE_STRUCT: return "STRUCT";
        case TYPE_UNION: return "UNION";
        case TYPE_ANY: return "ANY";
        default: return "ERROR";
    }
}

// 打印作用域符号
void print_currscope_symbols(TypeEnv *env) {
    if (!env || LIST_IS_EMPTY(&(env->scopeHead))) {
        printf("No active currScope\n");
        return;
    }
    
    printf("Scope symbols:\n");

    Symbol *currentSymbol = NULL;
    Node *posSymbol, *prevSymbol;
    Node *scopeHead = LIST_PEEK_TAIL(&(env->scopeHead));
    Scope *currentScope = LIST_ENTRY(scopeHead, Scope, scopeNode);
    LIST_FOREACH_SAFE(&(currentScope->symbols), posSymbol, prevSymbol)
    {
        currentSymbol = LIST_ENTRY(posSymbol, Symbol, symbolNode);
        printf("  %s : %s\n", currentSymbol->symbolStr, type_to_string(currentSymbol->type));
    }

    return;
}

/*
// 测试函数
void testTypeEnv() {
    TypeEnv *env = init_type_env();
    
    printf("=== Entering global currScope ===\n");
    enter_scope(env);
    
    printf("Adding symbols to global currScope...\n");
    add_symbol_to_scope(env, "x", TYPE_INT32);
    add_symbol_to_scope(env, "y", TYPE_FLOAT32);
    add_symbol_to_scope(env, "name", TYPE_STRING);
    add_symbol_to_scope(env, "x", TYPE_INT16);
    add_symbol_to_scope(env, "x", TYPE_INT8);
    add_symbol_to_scope(env, "name", TYPE_INT16);
    add_symbol_to_scope(env, "temp", TYPE_INT8);
    add_symbol_to_scope(env, "localVar", TYPE_FLOAT32);
    print_currscope_symbols(env);
    
    printf("\n=== Entering function currScope ===\n");
    enter_scope(env);
    
    printf("Adding symbols to function currScope...\n");
    add_symbol_to_scope(env, "localVar", TYPE_BOOLEAN);
    add_symbol_to_scope(env, "temp", TYPE_INT16);
    print_currscope_symbols(env);
    
    printf("\nLooking up symbols:\n");
    Symbol *x = find_symbol_in_scope(env, "x");
    printf("Found 'x': %s\n", x ? type_to_string(x->type) : "Not found");
    
    Symbol *local = find_symbol_in_scope(env, "localVar");
    printf("Found 'localVar': %s\n", local ? type_to_string(local->type) : "Not found");

    Symbol *temp = find_symbol_in_scope(env, "temp");
    printf("Found 'temp': %s\n", local ? type_to_string(temp->type) : "Not found");

    Symbol *unknown = find_symbol_in_scope(env, "unknown");
    printf("Found 'unknown': %s\n", unknown ? type_to_string(unknown->type) : "Not found");
    
    printf("\n=== Exiting function currScope ===\n");
    exit_scope(env);
    print_currscope_symbols(env);
    
    printf("\n=== Exiting global currScope ===\n");
    exit_scope(env);
    print_currscope_symbols(env);
    
    free_type_env(&env);
}

int main() {
    testTypeEnv();
    return 0;
}
    */