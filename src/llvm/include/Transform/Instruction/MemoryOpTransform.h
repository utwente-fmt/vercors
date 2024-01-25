#ifndef VCLLVM_MEMORYOPTRANSFORM_H
#define VCLLVM_MEMORYOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::serialize;

    void transformMemoryOp(llvm::Instruction &llvmInstruction,
                           col::Block &colBlock,
                           vcllvm::FunctionCursor &funcCursor);
}
#endif //VCLLVM_MEMORYOPTRANSFORM_H
