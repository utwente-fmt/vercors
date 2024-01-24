#ifndef VCLLVM_CASTOPTRANSFORM_H
#define VCLLVM_CASTOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::serialize;

    void convertCastOp(llvm::Instruction &llvmInstruction,
                       col::Block &colBlock,
                       vcllvm::FunctionCursor &funcCursor);
}
#endif //VCLLVM_CASTOPTRANSFORM_H
