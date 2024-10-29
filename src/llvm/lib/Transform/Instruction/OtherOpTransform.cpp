#include "Transform/Instruction/OtherOpTransform.h"
#include <llvm/IR/FMF.h>

#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::OtherOp";

void llvm2col::transformOtherOp(llvm::Instruction &llvmInstruction,
                                col::Block &colBlock,
                                pallas::FunctionCursor &funcCursor) {
    switch (llvm::Instruction::OtherOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::PHI:
        transformPhi(llvm::cast<llvm::PHINode>(llvmInstruction), funcCursor);
        break;
    case llvm::Instruction::ICmp:
        transformICmp(llvm::cast<llvm::ICmpInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::Call:
        transformCallExpr(llvm::cast<llvm::CallInst>(llvmInstruction), colBlock,
                          funcCursor);
        break;
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}

void llvm2col::transformPhi(llvm::PHINode &phiInstruction,
                            pallas::FunctionCursor &funcCursor) {
    col::Variable &varDecl = funcCursor.declareVariable(phiInstruction);
    for (auto &B : phiInstruction.blocks()) {
        // add assignment of the variable to target block
        col::Block &targetBlock =
            funcCursor.getOrSetLLVMBlock2LabeledColBlockEntry(*B).block;
        col::Assign &assignment = funcCursor.createPhiAssignment(
            phiInstruction, targetBlock, varDecl);
        // assign correct value by looking at the value-block pair of phi
        // instruction.
        col::Expr *value = assignment.mutable_value();
        llvm2col::transformAndSetExpr(
            funcCursor, phiInstruction,
            *phiInstruction.getIncomingValueForBlock(B), *value);
    }
}

void llvm2col::transformICmp(llvm::ICmpInst &icmpInstruction,
                             col::Block &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // we only support integer comparison
    if (not icmpInstruction.getOperand(0)->getType()->isIntegerTy()) {
        pallas::ErrorReporter::addError(SOURCE_LOC, "Unsupported compare type",
                                        icmpInstruction);
        return;
    }
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(icmpInstruction, colBlock);
    switch (llvm::ICmpInst::Predicate(icmpInstruction.getPredicate())) {
    case llvm::CmpInst::ICMP_EQ: {
        col::Eq &eq = *assignment.mutable_value()->mutable_eq();
        transformCmpExpr(icmpInstruction, eq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_NE: {
        col::Neq &neq = *assignment.mutable_value()->mutable_neq();
        transformCmpExpr(icmpInstruction, neq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SGT:
    case llvm::CmpInst::ICMP_UGT: {
        col::Greater &gt = *assignment.mutable_value()->mutable_greater();
        transformCmpExpr(icmpInstruction, gt, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SGE:
    case llvm::CmpInst::ICMP_UGE: {
        col::GreaterEq &geq = *assignment.mutable_value()->mutable_greater_eq();
        transformCmpExpr(icmpInstruction, geq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SLT:
    case llvm::CmpInst::ICMP_ULT: {
        col::Less &lt = *assignment.mutable_value()->mutable_less();
        transformCmpExpr(icmpInstruction, lt, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SLE:
    case llvm::CmpInst::ICMP_ULE: {
        col::LessEq &leq = *assignment.mutable_value()->mutable_less_eq();
        transformCmpExpr(icmpInstruction, leq, funcCursor);
        break;
    }
    default:
        pallas::ErrorReporter::addError(SOURCE_LOC, "Unknown ICMP predicate",
                                        icmpInstruction);
    }
}

void llvm2col::transformCmpExpr(llvm::CmpInst &cmpInstruction,
                                auto &colCompareExpr,
                                pallas::FunctionCursor &funcCursor) {
    transformBinExpr(cmpInstruction, colCompareExpr, funcCursor);
}

bool llvm2col::checkCallSupport(llvm::CallInst &callInstruction) {
    if (callInstruction.isIndirectCall()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Indirect calls are not supported", callInstruction);
        return false;
    }
    // tail recursion
    if (callInstruction.isMustTailCall()) {
        pallas::ErrorReporter::addError(SOURCE_LOC,
                                        "Tail call optimization not supported",
                                        callInstruction);
        return false;
    }
    // fast math
    if (callInstruction.getFastMathFlags().any()) {
        pallas::ErrorReporter::addError(SOURCE_LOC, "Fast math not supported",
                                        callInstruction);
        return false;
    }
    // return attributes
    for (auto &A : callInstruction.getAttributes().getRetAttrs()) {
        // TODO: Deal with these most of them do not affect the semantics we
        // care about so we could ignore them
        std::stringstream errorStream;
        errorStream << "Return attribute \"" << A.getAsString()
                    << "\" not supported";
        pallas::ErrorReporter::addWarning(SOURCE_LOC, errorStream.str(),
                                          callInstruction);
        return true;
    }
    // address space is platform dependent (unlikely to change semantics)
    // function attributes are just extra compiler information (no semanatic
    // changes)

    // operand bundles
    if (callInstruction.hasOperandBundles()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Operand bundles not supported", callInstruction);
        return false;
    }

    return true;
}

void llvm2col::transformCallExpr(llvm::CallInst &callInstruction,
                                 col::Block &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    if (!checkCallSupport(callInstruction) ||
        callInstruction.getCalledFunction() == nullptr)
        return;

    if (callInstruction.getCalledFunction()->isIntrinsic()) {
        // TODO: Deal with intrinsic functions
        return;
    }
    // allocate expression to host the function call in advance
    col::Expr *functionCallExpr;
    // if void function add an eval expression
    if (callInstruction.getType()->isVoidTy()) {
        col::Eval *eval = colBlock.add_statements()->mutable_eval();
        eval->set_allocated_origin(
            llvm2col::generateSingleStatementOrigin(callInstruction));
        functionCallExpr = eval->mutable_expr();
    } else { // else create an assignment
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        functionCallExpr = assignment.mutable_value();
    }
    // create actual invocation
    col::LlvmFunctionInvocation *invocation =
        functionCallExpr->mutable_llvm_function_invocation();
    invocation->set_allocated_blame(new col::Blame());
    // set origin
    invocation->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    // set function reference
    invocation->mutable_ref()->set_id(
        funcCursor.getFDResult(*callInstruction.getCalledFunction())
            .getFunctionId());
    // process function arguments
    for (auto &A : callInstruction.args()) {
        llvm2col::transformAndSetExpr(funcCursor, callInstruction, *A,
                                      *invocation->add_args());
    }
}
