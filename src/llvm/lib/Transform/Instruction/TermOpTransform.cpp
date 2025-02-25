#include "Transform/Instruction/TermOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/BlockUtils.h"
#include "Util/Exceptions.h"

#include <llvm/Analysis/CFG.h>
#include <llvm/Support/raw_ostream.h>

const std::string SOURCE_LOC = "Transform::Instruction::TermOp";

void llvm2col::transformTermOp(llvm::Instruction &llvmInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    switch (llvm::Instruction::TermOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::Ret:
        transformRet(cast<llvm::ReturnInst>(llvmInstruction), colBlock,
                     funcCursor);
        break;
    case llvm::Instruction::Br: {
        auto &llvmBranchInst = cast<llvm::BranchInst>(llvmInstruction);
        llvmBranchInst.isConditional()
            ? transformConditionalBranch(llvmBranchInst, colBlock, funcCursor)
            : transformUnConditionalBranch(llvmBranchInst, colBlock,
                                           funcCursor);
        break;
    }
    case llvm::Instruction::Unreachable: {
        transformUnreachable(cast<llvm::UnreachableInst>(llvmInstruction),
                             colBlock, funcCursor);
        break;
    }
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
        break;
    }
}

void llvm2col::transformRet(llvm::ReturnInst &llvmRetInstruction,
                            col::LlvmBasicBlock &colBlock,
                            pallas::FunctionCursor &funcCursor) {
    col::Return *returnStatement =
        colBlock.mutable_terminator()->mutable_return_();
    returnStatement->set_allocated_origin(
        generateSingleStatementOrigin(llvmRetInstruction));

    col::Expr *returnExpr = returnStatement->mutable_result();
    if (llvmRetInstruction.getReturnValue() == nullptr) {
        returnExpr->mutable_void_()->set_allocated_origin(
            generateVoidOperandOrigin(llvmRetInstruction));
    } else {
        llvm2col::transformAndSetExpr(funcCursor, llvmRetInstruction,
                                      *llvmRetInstruction.getReturnValue(),
                                      *returnExpr);
    }
}

void llvm2col::transformConditionalBranch(llvm::BranchInst &llvmBrInstruction,
                                          col::LlvmBasicBlock &colBlock,
                                          pallas::FunctionCursor &funcCursor) {
    col::Branch *colBranch = colBlock.mutable_terminator()->mutable_branch();
    colBranch->set_allocated_origin(
        generateSingleStatementOrigin(llvmBrInstruction));

    /*
     * I hear you think, why query the 2nd operand? wouldn't that be the false
     * branch i.e the else branch? While any logical implementation of getting
     * operands would give the operands in order, the branch instruction is no
     * ordinary instruction. For you see to get the branch argument we use the
     * 0th index (so far so good), for the true evaluation of the branch
     * instruction we use the 2nd index (uhhh okay, we might be skipping an
     * index?) and the false evaluation of the branch instruction we use the 1st
     * index (WHAT!?!?)
     *
     * Visualized:
     * br i1 %var, label %yay, label %nay
     *      0           2           1
     *
     * Just smile and wave, don't question LLVM.
     */
    auto *llvmTrueBlock =
        cast<llvm::BasicBlock>(llvmBrInstruction.getOperand(2));
    auto *llvmFalseBlock =
        cast<llvm::BasicBlock>(llvmBrInstruction.getOperand(1));

    // true branch
    col::Tuple2_VctColAstExpr_VctColAstStatement *colTrueBranch =
        colBranch->add_branches();
    transformAndSetExpr(funcCursor, llvmBrInstruction,
                        *llvmBrInstruction.getCondition(),
                        *colTrueBranch->mutable_v1());

    // get or pre-generate target labeled block
    col::LlvmBasicBlock &labeledTrueColBlock =
        funcCursor.getOrSetLLVMBlock2ColBlockEntry(*llvmTrueBlock);
    // goto statement to true block
    col::Goto *trueGoto = colTrueBranch->mutable_v2()->mutable_goto_();
    trueGoto->mutable_lbl()->set_id(labeledTrueColBlock.label().id());
    // set origin for goto to true block
    trueGoto->set_allocated_origin(
        generateSingleStatementOrigin(llvmBrInstruction));

    // false branch
    col::Tuple2_VctColAstExpr_VctColAstStatement *colFalseBranch =
        colBranch->add_branches();
    // set conditional (which is a true constant as else == else if(true)))
    col::BooleanValue *elseCondition =
        colFalseBranch->mutable_v1()->mutable_boolean_value();
    elseCondition->set_value(true);
    // set origin of else condition
    elseCondition->set_allocated_origin(generateOperandOrigin(
        llvmBrInstruction, *llvmBrInstruction.getCondition()));

    col::LlvmBasicBlock &labeledFalseColBlock =
        funcCursor.getOrSetLLVMBlock2ColBlockEntry(*llvmFalseBlock);
    // goto statement to false block
    col::Goto *falseGoto = colFalseBranch->mutable_v2()->mutable_goto_();
    falseGoto->mutable_lbl()->set_id(labeledFalseColBlock.label().id());
    // set origin for goto to false block
    falseGoto->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmBrInstruction));

    // Transform the blocks of the branches
    transformLLVMBlock(*llvmTrueBlock, funcCursor);
    transformLLVMBlock(*llvmFalseBlock, funcCursor);
}

void llvm2col::transformUnConditionalBranch(
    llvm::BranchInst &llvmBrInstruction, col::LlvmBasicBlock &colBlock,
    pallas::FunctionCursor &funcCursor) {
    // get llvm target block
    auto *llvmTargetBlock =
        cast<llvm::BasicBlock>(llvmBrInstruction.getOperand(0));
    // transform llvm targetBlock
    transformLLVMBlock(*llvmTargetBlock, funcCursor);
    // get or pre generate target labeled block
    col::LlvmBasicBlock &labeledColBlock =
        funcCursor.getOrSetLLVMBlock2ColBlockEntry(*llvmTargetBlock);
    // create goto to target labeled block
    col::Goto *colGoto = colBlock.mutable_terminator()->mutable_goto_();
    colGoto->mutable_lbl()->set_id(labeledColBlock.label().id());
    // set origin of goto statement
    colGoto->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmBrInstruction));
}

void llvm2col::transformUnreachable(
    llvm::UnreachableInst &llvmUnreachableInstruction,
    col::LlvmBasicBlock &colBlock, pallas::FunctionCursor &funcCursor) {
    col::LlvmBranchUnreachable *unreachableStatement =
        colBlock.mutable_terminator()->mutable_llvm_branch_unreachable();
    unreachableStatement->set_allocated_origin(
        generateSingleStatementOrigin(llvmUnreachableInstruction));
    unreachableStatement->set_allocated_blame(new col::Blame());
}
