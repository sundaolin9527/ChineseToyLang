#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "type.h"

// 清理类型环境
void cleanupTypeEnv(TypeEnv *env) {
    if (!env || !env->currScope) return;
    if (env->idle_scope != NULL)
    {
        LIST_FREE(env->idle_scope, Scope, scopeNode);
    }

    if (env->idle_symbol != NULL)
    {
        LIST_FREE(env->idle_symbol, Symbol, symbolNode);
    } 
    
    if (env->currScope->symbols != NULL)
    {
        LIST_FREE(env->currScope->symbols, Symbol, symbolNode);
    }

    LIST_FREE((env->currScope), Scope, scopeNode);
    return;
}

// 从空闲链表获取或创建新作用域
static Scope* getOrCreateScope(TypeEnv *env) {
    if (env->idle_scope) {
        Scope *currScope = env->idle_scope;
        env->idle_scope = (Scope*)(currScope->scopeNode.next);
        currScope->symbols = NULL;
        INIT_LIST_HEAD(&currScope->scopeNode);
        return currScope;
    }
    
    Scope *currScope = (Scope*)malloc(sizeof(Scope));
    if (currScope) {
        INIT_LIST_HEAD(&currScope->scopeNode);
        currScope->symbols = NULL;
    }
    return currScope;
}

// 从空闲链表获取或创建新符号
static Symbol* getOrCreateSymbol(TypeEnv *env) {
    if (env->idle_symbol) {
        Symbol *symbol = env->idle_symbol;
        env->idle_symbol = (Symbol*)symbol->symbolNode.next;
        symbol->type = TYPE_UNKNOWN;
        INIT_LIST_HEAD(&symbol->symbolNode);
        return symbol;
    }
    
    Symbol *symbol = (Symbol*)malloc(sizeof(Symbol));
    if (symbol) {
        INIT_LIST_HEAD(&symbol->symbolNode);
        symbol->type = TYPE_UNKNOWN;
    }
    return symbol;
}

// 初始化类型环境
TypeEnv *initTypeEnv() {
    TypeEnv *typeEnv = (TypeEnv*)malloc(sizeof(TypeEnv));
    if (typeEnv) {
        memset(typeEnv, 0, sizeof(TypeEnv));
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
    if (env->currScope) {
        LIST_INSERT_TAIL(&(env->currScope->scopeNode) , &newScope->scopeNode);
    }
    env->currScope = newScope;
    return;
}

// 离开当前作用域
void exitScope(TypeEnv *env) {
    if (!env || !env->currScope) return;
    
    Scope *currentScope = env->currScope;
    
    // 将符号移动到空闲链表
    Symbol *symbol = currentScope->symbols;
    if (symbol != NULL){
        if (env->idle_symbol != NULL)
        {
            LIST_INSERT_TAIL(&(env->idle_symbol->symbolNode), &symbol->symbolNode);
        }
        else
        {
            env->idle_symbol = symbol;
        }
    }
    
    // 更新作用域栈
    if (currentScope->scopeNode.next != &currentScope->scopeNode) {
        LIST_DELETE(&currentScope->scopeNode); //只是从env->currScope链上摘除，并没有释放内存
    } else {
        //当前作用域只有1个
        env->currScope = NULL;
    }

    if (env->idle_scope != NULL)
    {
        // 将作用域移动到空闲链表
        LIST_INSERT_TAIL(&(env->idle_scope->scopeNode), &currentScope->scopeNode);
    }
    else
    {
        env->idle_scope = currentScope;
    }
    
    return;
}

// 在当前作用域添加符号
bool addSymbolToScope(TypeEnv *env, const char *name, Type type) {
    if (!env || !env->currScope || !name) return false;
    
    // 检查符号是否已存在
    Symbol *head = env->currScope->symbols;
    Symbol *existing = head;
    if (head != NULL)
    {
        do{
            if (strncmp(existing->symbolStr, name, strlen(name)+1) == 0 ||
                existing->type == type) {
                return true; // 符号已存在
            }
            existing = (Symbol*)existing->symbolNode.next;
        }while(existing != head);
    }

    // 创建新符号
    Symbol *symbol = getOrCreateSymbol(env);
    if (!symbol) return false;
    strncpy(symbol->symbolStr, name, strlen(name)+1);
    symbol->type = type;
    
    // 添加到作用域符号表
    if (env->currScope->symbols) {
        LIST_INSERT_TAIL(&(env->currScope->symbols->symbolNode), &symbol->symbolNode);
    }
    else
    {
        env->currScope->symbols = symbol;
    }
    
    return true;
}

// 查找符号（从当前作用域向外查找）
Symbol* findSymbolInScope(TypeEnv *env, const char *name) {
    if (!env || !name) return NULL;
    
    Scope *currentScope = env->currScope;
    Scope *currentScopeHead = currentScope;
    Symbol *symbol = NULL;
    Symbol *symbolHead = NULL;
    do{
        symbol = currentScope->symbols;
        symbolHead = currentScope->symbols;
        do{
            if (strncmp(symbol->symbolStr, name, strlen(name) + 1) == 0) {
                return symbol;
            }
            symbol = (Symbol*)symbol->symbolNode.next;
        } while(symbol != symbolHead);
        
        // 移动到上一层作用域
        if (currentScope->scopeNode.next != &currentScope->scopeNode) {
            currentScope = (Scope*)(currentScope->scopeNode.prev);
        } else {
            currentScope = NULL;
        }
    } while(currentScope != currentScopeHead);

    return NULL;
}

void freeSymbol(Symbol *symbol, TypeEnv *env) {
    if (!symbol || !env) return;
    LIST_FREE(symbol, Symbol, symbolNode);
    symbol = NULL;
    return;
}

// 释放整个作用域(Scope)及其所有符号
void freeScope(Scope *scope, TypeEnv *env) {
    if (!scope || !env) return;
    LIST_FREE(scope, Scope, scopeNode);
    scope = NULL;
    return;
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

// 打印当前作用域符号
void printCurrScopeSymbols(TypeEnv *env) {
    if (!env || !env->currScope) {
        printf("No active currScope\n");
        return;
    }
    
    printf("Scope symbols:\n");
    Symbol *symbol = env->currScope->symbols;
    Symbol *symbolHead = symbol;
    do {
        printf("  %s : %s\n", symbol->symbolStr, typeToString(symbol->type));
        symbol = (Symbol*)symbol->symbolNode.next;
    } while(symbol != symbolHead);
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
    
    Symbol *unknown = findSymbolInScope(env, "unknown");
    printf("Found 'unknown': %s\n", unknown ? typeToString(unknown->type) : "Not found");
    
    printf("\n=== Exiting function currScope ===\n");
    exitScope(env);
    printCurrScopeSymbols(env);
    
    printf("\n=== Exiting global currScope ===\n");
    exitScope(env);
    
    freeTypeEnv(&env);
}

int main() {
    testTypeEnv();
    return 0;
}
    