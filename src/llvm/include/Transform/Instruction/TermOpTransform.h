#ifndef VCLLVM_TERMOPTRANSFORM_H
#define VCLLVM_TERMOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::serialize;

    void transformTermOp(llvm::Instruction &llvmInstruction,
                         col::Block &colBlock,
                         vcllvm::FunctionCursor &funcCursor);

    void transformRet(llvm::ReturnInst &llvmRetInstruction,
                      col::Block &colBlock,
                      vcllvm::FunctionCursor &funcCursor);

    void transformConditionalBranch(llvm::BranchInst &llvmBrInstruction,
                                    col::Block &colBlock,
                                    vcllvm::FunctionCursor &funcCursor);

    void transformUnConditionalBranch(llvm::BranchInst &llvmBrInstruction,
                                      col::Block &colBlock,
                                      vcllvm::FunctionCursor &funcCursor);
}
#endif //VCLLVM_TERMOPTRANSFORM_H
