#ifndef VCLLVM_BINARYOPTRANSFORM_H
#define VCLLVM_BINARYOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::ast;


    void transformBinaryOp(llvm::Instruction &llvmInstruction,
                           col::Block &colBlock,
                           vcllvm::FunctionCursor &funcCursor);

}
#endif //VCLLVM_BINARYOPTRANSFORM_H
