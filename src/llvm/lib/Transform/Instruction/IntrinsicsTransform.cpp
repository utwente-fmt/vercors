#include "Transform/Instruction/IntrinsicsTransform.h"
#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/BlockUtils.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>

const std::string SOURCE_LOC = "Transform::Instruction::Intrinsics";

void llvm2col::transformIntrinsic(llvm::CallInst &callInstruction,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &funcCursor) {
    llvm::Function *intrFunc = callInstruction.getCalledFunction();
    switch (intrFunc->getIntrinsicID()) {
    case llvm::Intrinsic::trap:
        transformTrap(callInstruction, colBlock, funcCursor);
        break;
    case llvm::Intrinsic::expect:
        transformExpect(callInstruction, colBlock, funcCursor);
        break;
    case llvm::Intrinsic::sadd_with_overflow:
        transformAddWithOverflow(callInstruction, colBlock, funcCursor, true);
        break;
    case llvm::Intrinsic::uadd_with_overflow:
        transformAddWithOverflow(callInstruction, colBlock, funcCursor, false);
        break;
    case llvm::Intrinsic::ssub_with_overflow:
        transformSubWithOverflow(callInstruction, colBlock, funcCursor, true);
        break;
    case llvm::Intrinsic::usub_with_overflow:
        transformSubWithOverflow(callInstruction, colBlock, funcCursor, false);
        break;
    case llvm::Intrinsic::smul_with_overflow:
        transformMultWithOverflow(callInstruction, colBlock, funcCursor, true);
        break;
    case llvm::Intrinsic::umul_with_overflow:
        transformMultWithOverflow(callInstruction, colBlock, funcCursor, false);
        break;
    case llvm::Intrinsic::dbg_value:
    case llvm::Intrinsic::dbg_declare:
    case llvm::Intrinsic::dbg_assign:
        // Ignore debug-intrinsics
        break;
    case llvm::Intrinsic::lifetime_start:
    case llvm::Intrinsic::lifetime_end:
        // Ignore lifetime intrinsics
        break;
    case llvm::Intrinsic::memset:
        transformMemset(callInstruction, colBlock, funcCursor);
        break;
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, callInstruction);
    }

    // TODO: Deal with more intrinsic functions
}

void llvm2col::transformTrap(llvm::CallInst &callInstruction,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // Transform @llvm.trap() --> assert false
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::VctAssert *a = body.add_statements()->mutable_vct_assert();
    a->set_allocated_blame(new col::Blame());
    a->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    col::BooleanValue *falseConst = a->mutable_res()->mutable_boolean_value();
    falseConst->set_value(false);
    falseConst->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
}

void llvm2col::transformExpect(llvm::CallInst &callInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    // Transform %x2 = call XXX @llvm.expect.XXX(%x1, YYY) --> x2 = x1;
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    // auto *assignExpr = assignment.mutable_value();
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *assignment.mutable_value());
}

void llvm2col::transformAddWithOverflow(llvm::CallInst &callInstruction,
                                        col::LlvmBasicBlock &colBlock,
                                        pallas::FunctionCursor &funcCursor,
                                        bool sign) {
    llvm::Function *intrFunc = callInstruction.getCalledFunction();
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::Variable &targetVar = funcCursor.declareVariable(callInstruction);
    col::LlvmAddWithOverflow *add =
        body.add_statements()->mutable_llvm_add_with_overflow();
    add->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    add->set_allocated_blame(new col::Blame());
    // signed
    add->set_signed_(sign);
    // target
    col::Local *targetLocal = add->mutable_target()->mutable_local();
    targetLocal->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(callInstruction));
    targetLocal->mutable_ref()->set_id(targetVar.id());
    // left
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *add->mutable_left());
    // right
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *add->mutable_right());
}

void llvm2col::transformSubWithOverflow(llvm::CallInst &callInstruction,
                                        col::LlvmBasicBlock &colBlock,
                                        pallas::FunctionCursor &funcCursor,
                                        bool sign) {
    llvm::Function *intrFunc = callInstruction.getCalledFunction();
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::Variable &targetVar = funcCursor.declareVariable(callInstruction);
    col::LlvmSubWithOverflow *sub =
        body.add_statements()->mutable_llvm_sub_with_overflow();
    sub->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    sub->set_allocated_blame(new col::Blame());
    // signed
    sub->set_signed_(sign);
    // target
    col::Local *targetLocal = sub->mutable_target()->mutable_local();
    targetLocal->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(callInstruction));
    targetLocal->mutable_ref()->set_id(targetVar.id());
    // left
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *sub->mutable_left());
    // right
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *sub->mutable_right());
}

void llvm2col::transformMultWithOverflow(llvm::CallInst &callInstruction,
                                         col::LlvmBasicBlock &colBlock,
                                         pallas::FunctionCursor &funcCursor,
                                         bool sign) {
    llvm::Function *intrFunc = callInstruction.getCalledFunction();
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::Variable &targetVar = funcCursor.declareVariable(callInstruction);
    col::LlvmMultWithOverflow *mult =
        body.add_statements()->mutable_llvm_mult_with_overflow();
    mult->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    mult->set_allocated_blame(new col::Blame());
    // signed
    mult->set_signed_(sign);
    // target
    col::Local *targetLocal = mult->mutable_target()->mutable_local();
    targetLocal->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(callInstruction));
    targetLocal->mutable_ref()->set_id(targetVar.id());
    // left
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *mult->mutable_left());
    // right
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *mult->mutable_right());
}

void llvm2col::transformMemset(llvm::CallInst &callInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    llvm::Function *intrFunc = callInstruction.getCalledFunction();
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::Variable &targetVar = funcCursor.declareVariable(callInstruction);
    col::LlvmMemset *memset = body.add_statements()->mutable_llvm_memset();
    memset->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    memset->set_allocated_blame(new col::Blame());
    // dest
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *memset->mutable_dest());
    // value
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *memset->mutable_value());
    // len
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(2),
                                  *memset->mutable_len());
    // volatile
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(3),
                                  *memset->mutable_volatile_());
}
