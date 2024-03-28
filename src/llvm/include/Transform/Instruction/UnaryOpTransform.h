#ifndef VCLLVM_UNARYOPTRANSFORM_H
#define VCLLVM_UNARYOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::ast;

    void transformUnaryOp(llvm::Instruction &llvmInstruction,
                          col::Block &colBlock,
                          vcllvm::FunctionCursor &funcCursor);
}
#endif //VCLLVM_UNARYOPTRANSFORM_H