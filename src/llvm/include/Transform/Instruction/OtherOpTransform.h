#ifndef VCLLVM_OTHEROPTRANSFORM_H
#define VCLLVM_OTHEROPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2Col {
    namespace col = vct::col::serialize;

    void transformOtherOp(llvm::Instruction &llvmInstruction,
                          col::Block &colBlock,
                          vcllvm::FunctionCursor &funcCursor);
    /**
     * Phi nodes get transformed retroactively by creating a variable declaration and retroactively assign the variable
     * in each originating COL block of each phi pair.
     * @param phiInstruction
     * @param funcCursor
     */
    void transformPhi(llvm::PHINode &phiInstruction, vcllvm::FunctionCursor &funcCursor);

    void transformICmp(llvm::ICmpInst &icmpInstruction,
                       col::Block &colBlock,
                       vcllvm::FunctionCursor &funcCursor);
    /**
     * Transforms the common part of all compare instructions (the argument pair). Currently only used by transformIcmp
     * but could also be used in the future by for example an FCMP transformation.
     * @param cmpInstruction
     * @param colCompareExpr
     * @param funcCursor
     */
    void transformCmpExpr(llvm::CmpInst &cmpInstruction,
                          auto &colCompareExpr,
                          vcllvm::FunctionCursor &funcCursor);

    void transformCallExpr(llvm::CallInst &callInstruction,
                           col::Block &colBlock,
                           vcllvm::FunctionCursor &funcCursor);
}

#endif //VCLLVM_OTHEROPTRANSFORM_H
