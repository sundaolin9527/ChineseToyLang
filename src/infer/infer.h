#ifndef INFER_H
#define INFER_H

#include "../type/type.h"
#include "../frontend/ast/ast.h"

Type infer_type(TypeEnv *env, ASTNode* node);

#endif // INFER_H