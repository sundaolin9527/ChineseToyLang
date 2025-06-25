#ifndef IR_EMITTER_H
#define IR_EMITTER_H
#include "Emitter.h"


void EmitProgram(llvm::IRBuilder<>& Builder,
                llvm::LLVMContext& Context,
                llvm::Module* Module,
                const StmtSequence& program);

#endif /* IR_EMITTER_H */