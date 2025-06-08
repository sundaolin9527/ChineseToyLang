#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "type.h"

// 清理类型环境
void cleanupTypeEnv(TypeEnv *env) {
    if (!env) return;

    LIST_FREE(&(env->scopeHead), Scope, scopeNode);
    LIST_FREE(&(env->idle_scope), Scope, scopeNode);
    LIST_FREE(&(env->idle_symbol), Symbol, symbolNode);
    return;
}

// 从空闲链表获取或创建新作用域
static Scope* getOrCreateScope(TypeEnv *env) {
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
static Symbol* getOrCreateSymbol(TypeEnv *env) {
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
TypeEnv *initTypeEnv() {
    TypeEnv *typeEnv = (TypeEnv*)malloc(sizeof(TypeEnv));
    if (typeEnv) {
        memset(typeEnv, 0, sizeof(TypeEnv));
        INIT_LIST_HEAD(&(typeEnv->scopeHead));
        INIT_LIST_HEAD(&(typeEnv->idle_scope));
        INIT_LIST_HEAD(&(typeEnv->idle_symbol));
    }
    return typeEnv;
}

void freeTypeEnv(TypeEnv **typeEnv) {
    if (!typeEnv|| !(*typeEnv))
    {
        return;
    }

    cleanupTypeEnv(*typeEnv);
    free(*typeEnv);
    *typeEnv = NULL;
    return;
}

// 进入新作用域
void enterScope(TypeEnv *env) {
    if (!env) return;
    
    Scope *newScope = getOrCreateScope(env);
    if (!newScope) return;

    // 将新作用域压入栈顶
    LIST_INSERT_TAIL(&(env->scopeHead) , &(newScope->scopeNode));
    return;
}

// 离开当前作用域
void exitScope(TypeEnv *env) {
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
bool addSymbolToScope(TypeEnv *env, const char *name, Type type) {
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
    Symbol *symbol = getOrCreateSymbol(env);
    if (!symbol) return false;
    strncpy(symbol->symbolStr, name, strlen(name)+1);
    symbol->type = type;

    // 添加到作用域符号表
    LIST_INSERT_TAIL(&(currentScope->symbols), &(symbol->symbolNode));
    
    return true;
}

// 查找符号（从当前作用域向外查找）
Symbol* findSymbolInScope(TypeEnv *env, const char *name) {
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
const char* typeToString(Type type) {
    switch (type) {
        case TYPE_UNKNOWN: return "UNKNOWN";
        case TYPE_INT8: return "INT8";
        case TYPE_INT16: return "INT16";
        case TYPE_INT32: return "INT32";
        case TYPE_INT64: return "INT64";
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
void printCurrScopeSymbols(TypeEnv *env) {
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
        printf("  %s : %s\n", currentSymbol->symbolStr, typeToString(currentSymbol->type));
    }

    return;
}


// 测试函数
void testTypeEnv() {
    TypeEnv *env = initTypeEnv();
    
    printf("=== Entering global currScope ===\n");
    enterScope(env);
    
    printf("Adding symbols to global currScope...\n");
    addSymbolToScope(env, "x", TYPE_INT32);
    addSymbolToScope(env, "y", TYPE_FLOAT32);
    addSymbolToScope(env, "name", TYPE_STRING);
    addSymbolToScope(env, "x", TYPE_INT16);
    addSymbolToScope(env, "x", TYPE_INT8);
    addSymbolToScope(env, "name", TYPE_INT16);
    addSymbolToScope(env, "temp", TYPE_INT8);
    addSymbolToScope(env, "localVar", TYPE_FLOAT32);
    printCurrScopeSymbols(env);
    
    printf("\n=== Entering function currScope ===\n");
    enterScope(env);
    
    printf("Adding symbols to function currScope...\n");
    addSymbolToScope(env, "localVar", TYPE_BOOLEAN);
    addSymbolToScope(env, "temp", TYPE_INT16);
    printCurrScopeSymbols(env);
    
    printf("\nLooking up symbols:\n");
    Symbol *x = findSymbolInScope(env, "x");
    printf("Found 'x': %s\n", x ? typeToString(x->type) : "Not found");
    
    Symbol *local = findSymbolInScope(env, "localVar");
    printf("Found 'localVar': %s\n", local ? typeToString(local->type) : "Not found");

    Symbol *temp = findSymbolInScope(env, "temp");
    printf("Found 'temp': %s\n", local ? typeToString(temp->type) : "Not found");

    Symbol *unknown = findSymbolInScope(env, "unknown");
    printf("Found 'unknown': %s\n", unknown ? typeToString(unknown->type) : "Not found");
    
    printf("\n=== Exiting function currScope ===\n");
    exitScope(env);
    printCurrScopeSymbols(env);
    
    printf("\n=== Exiting global currScope ===\n");
    exitScope(env);
    printCurrScopeSymbols(env);
    
    freeTypeEnv(&env);
}

int main() {
    testTypeEnv();
    return 0;
}