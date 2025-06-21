#ifndef INFER_H
#define INFER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "fronted/type/type.h"
#include "fronted/ast/ast.h"

ValueType infer_type(TypeEnv *env, ASTNode* node);

#ifdef __cplusplus
}
#endif
#endif // INFER_H
