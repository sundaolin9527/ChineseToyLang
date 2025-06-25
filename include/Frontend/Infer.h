#ifndef INFER_H
#define INFER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "Basic/Types.h"
#include "Frontend/Ast.h"

TypeKind infer_type(TypeEnv *env, ASTNode* node);

#ifdef __cplusplus
}
#endif
#endif // INFER_H
