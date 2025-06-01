#include "type.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 创建基础类型
Type* new_type(TypeKind kind) {
    Type* t = malloc(sizeof(Type));
    t->kind = kind;
    return t;
}

// 创建函数类型
Type* new_function_type(Type* return_type, Type** params, int param_count) {
    Type* t = new_type(TYPE_FUNCTION);
    t->meta.func.return_type = return_type;
    t->meta.func.param_types = malloc(param_count * sizeof(Type*));
    memcpy(t->meta.func.param_types, params, param_count * sizeof(Type*));
    t->meta.func.param_count = param_count;
    return t;
}

// 创建数组类型
Type* new_array_type(Type* element_type, int size) {
    Type* t = new_type(TYPE_ARRAY);
    t->meta.array.element_type = element_type;
    t->meta.array.size = size;
    return t;
}

// 释放类型
void free_type(Type* type) {
    if (!type) return;
    
    if (type->kind == TYPE_FUNCTION) {
        free(type->meta.func.param_types);
        free_type(type->meta.func.return_type);
    } else if (type->kind == TYPE_ARRAY) {
        free_type(type->meta.array.element_type);
    }
    
    free(type);
}

// 创建类型环境
TypeEnv* new_type_env(const char* name, Type* type, TypeEnv* next) {
    TypeEnv* env = malloc(sizeof(TypeEnv));
    env->name = strdup(name);
    env->type = type;
    env->next = next;
    return env;
}

// 查找类型
Type* lookup_type(TypeEnv* env, const char* name) {
    while (env) {
        if (strcmp(env->name, name) == 0) {
            return env->type;
        }
        env = env->next;
    }
    return NULL;
}

// 扩展类型环境
TypeEnv* extend_type_env(TypeEnv* env, const char* name, Type* type) {
    return new_type_env(name, type, env);
}

// 释放类型环境
void free_type_env(TypeEnv* env) {
    while (env) {
        TypeEnv* next = env->next;
        free(env->name);
        free_type(env->type);
        free(env);
        env = next;
    }
}

// 类型相等判断
bool type_equal(Type* t1, Type* t2) {
    if (!t1 || !t2) return false;
    if (t1->kind != t2->kind) return false;
    
    switch (t1->kind) {
        case TYPE_FUNCTION:
            if (!type_equal(t1->meta.func.return_type, t2->meta.func.return_type) ||
                t1->meta.func.param_count != t2->meta.func.param_count) {
                return false;
            }
            for (int i = 0; i < t1->meta.func.param_count; i++) {
                if (!type_equal(t1->meta.func.param_types[i], t2->meta.func.param_types[i])) {
                    return false;
                }
            }
            return true;
            
        case TYPE_ARRAY:
            return type_equal(t1->meta.array.element_type, t2->meta.array.element_type) &&
                   t1->meta.array.size == t2->meta.array.size;
            
        default:
            return true;
    }
}

// 类型联合(寻找共同类型)
Type* type_join(Type* t1, Type* t2) {
    if (!t1 || !t2) return NULL;
    if (type_equal(t1, t2)) return t1;
    
    // 数值类型提升
    if ((t1->kind == TYPE_INT || t1->kind == TYPE_FLOAT) &&
        (t2->kind == TYPE_INT || t2->kind == TYPE_FLOAT)) {
        return new_type(TYPE_FLOAT);
    }
    
    return new_type(TYPE_UNKNOWN);
}

// 类型转字符串
const char* type_to_string(Type* type) {
    if (!type) return "NULL";
    
    static char buffer[256];
    
    switch (type->kind) {
        case TYPE_UNKNOWN: return "UNKNOWN";
        case TYPE_INT: return "INT";
        case TYPE_FLOAT: return "FLOAT";
        case TYPE_BOOL: return "BOOL";
        case TYPE_STRING: return "STRING";
        case TYPE_VOID: return "VOID";
            
        case TYPE_FUNCTION: {
            char* p = buffer;
            p += sprintf(p, "FUNC(");
            
            for (int i = 0; i < type->meta.func.param_count; i++) {
                if (i > 0) p += sprintf(p, ", ");
                p += sprintf(p, "%s", type_to_string(type->meta.func.param_types[i]));
            }
            
            sprintf(p, ")->%s", type_to_string(type->meta.func.return_type));
            return buffer;
        }
            
        case TYPE_ARRAY: {
            if (type->meta.array.size >= 0) {
                sprintf(buffer, "ARRAY[%d]%s", 
                       type->meta.array.size, 
                       type_to_string(type->meta.array.element_type));
            } else {
                sprintf(buffer, "ARRAY[]%s", 
                       type_to_string(type->meta.array.element_type));
            }
            return buffer;
        }
            
        default: return "UNRECOGNIZED";
    }
}