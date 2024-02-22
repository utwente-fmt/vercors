#include "Transform/Instruction/TermOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::TermOp";

void llvm2col::transformTermOp(llvm::Instruction &llvmInstruction,
                               col::Block &colBlock,
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
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
        break;
    }
}

void llvm2col::transformRet(llvm::ReturnInst &llvmRetInstruction,
                            col::Block &colBlock,
                            pallas::FunctionCursor &funcCursor) {
    col::Return *returnStatement = colBlock.add_statements()->mutable_return_();
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
                                          col::Block &colBlock,
                                          pallas::FunctionCursor &funcCursor) {
    col::Branch *colBranch = colBlock.add_statements()->mutable_branch();
    colBranch->set_allocated_origin(
        generateSingleStatementOrigin(llvmBrInstruction));
    // pre-declare completion because the final branch statement is already
    // present
    funcCursor.complete(colBlock);
    // true branch
    col::Tuple2_VctColAstExpr_VctColAstStatement *colTrueBranch =
        colBranch->add_branches();
    // set conditional
    transformAndSetExpr(funcCursor, llvmBrInstruction,
                        *llvmBrInstruction.getCondition(),
                        *colTrueBranch->mutable_v1());
    // get or pre-generate target labeled block
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
    // transform llvm true block
    transformLLVMBlock(*llvmTrueBlock, funcCursor);
    pallas::LabeledColBlock labeledTrueColBlock =
        funcCursor.getOrSetLLVMBlock2LabeledColBlockEntry(*llvmTrueBlock);
    // goto statement to true block
    col::Goto *trueGoto = colTrueBranch->mutable_v2()->mutable_goto_();
    trueGoto->mutable_lbl()->set_id(labeledTrueColBlock.label.decl().id());
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
    // get llvm block targeted by the llvm branch
    auto *llvmFalseBlock =
        cast<llvm::BasicBlock>(llvmBrInstruction.getOperand(1));
    // transform llvm falseBlock
    transformLLVMBlock(*llvmFalseBlock, funcCursor);
    // get or pre-generate target labeled block
    pallas::LabeledColBlock labeledFalseColBlock =
        funcCursor.getOrSetLLVMBlock2LabeledColBlockEntry(*llvmFalseBlock);
    // goto statement to false block
    col::Goto *falseGoto = colFalseBranch->mutable_v2()->mutable_goto_();
    falseGoto->mutable_lbl()->set_id(labeledFalseColBlock.label.decl().id());
    // set origin for goto to false block
    falseGoto->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmBrInstruction));
}

void llvm2col::transformUnConditionalBranch(
    llvm::BranchInst &llvmBrInstruction, col::Block &colBlock,
    pallas::FunctionCursor &funcCursor) {
    // get llvm target block
    auto *llvmTargetBlock =
        cast<llvm::BasicBlock>(llvmBrInstruction.getOperand(0));
    // transform llvm targetBlock
    transformLLVMBlock(*llvmTargetBlock, funcCursor);
    // get or pre generate target labeled block
    pallas::LabeledColBlock labeledColBlock =
        funcCursor.getOrSetLLVMBlock2LabeledColBlockEntry(*llvmTargetBlock);
    // create goto to target labeled block
    col::Goto *colGoto = colBlock.add_statements()->mutable_goto_();
    colGoto->mutable_lbl()->set_id(labeledColBlock.label.decl().id());
    // set origin of goto statement
    colGoto->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmBrInstruction));
    // pre-declare completion because the final goto is already present
    funcCursor.complete(colBlock);
}
