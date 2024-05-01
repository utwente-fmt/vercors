#ifndef VCLLVM_FUNCLETPADOPTRANSFORM_H
#define VCLLVM_FUNCLETPADOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::ast;

    void transformFuncletPadOp(llvm::Instruction &llvmInstruction,
                               col::Block &colBlock,
                               vcllvm::FunctionCursor &funcCursor);
}
#endif //VCLLVM_FUNCLETPADOPTRANSFORM_H
