#include "Transform/Instruction/OtherOpTransform.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/FMF.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>

#include "IRSpec/PallasSpecDecoding.h"
#include "Passes/Function/ExprWrapperMapper.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Module/StructTDeclarer.h"
#include "Transform/BlockTransform.h"
#include "Transform/Instruction/IntrinsicsTransform.h"
#include "Transform/SpecStatementTransform.h"
#include "Transform/Transform.h"
#include "Transform/WrapperCallTransform.h"
#include "Util/BlockUtils.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"

const std::string SOURCE_LOC = "Transform::Instruction::OtherOp";

void llvm2col::transformOtherOp(llvm::Instruction &llvmInstruction,
                                col::LlvmBasicBlock &colBlock,
                                pallas::FunctionCursor &funcCursor) {
    switch (llvm::Instruction::OtherOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::PHI:
        transformPhi(llvm::cast<llvm::PHINode>(llvmInstruction), colBlock,
                     funcCursor);
        break;
    case llvm::Instruction::Select:
        transformSelect(llvm::cast<llvm::SelectInst>(llvmInstruction), colBlock,
                        funcCursor);
        break;
    case llvm::Instruction::ICmp:
        transformICmp(llvm::cast<llvm::ICmpInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::FCmp:
        transformFCmp(llvm::cast<llvm::FCmpInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::Call:
        transformCallExpr(llvm::cast<llvm::CallInst>(llvmInstruction), colBlock,
                          funcCursor);
        break;
    case llvm::Instruction::ExtractValue:
        transformExtractValueInst(
            llvm::cast<llvm::ExtractValueInst>(llvmInstruction), colBlock,
            funcCursor);
        break;
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}

void llvm2col::transformSelect(llvm::SelectInst &selectInst,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(selectInst, colBlock);
    col::Select &select = *assignment.mutable_value()->mutable_select();
    select.set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(selectInst));
    // Condition
    llvm2col::transformAndSetExpr(funcCursor, selectInst,
                                  *selectInst.getCondition(),
                                  *select.mutable_condition());
    // True
    llvm2col::transformAndSetExpr(funcCursor, selectInst,
                                  *selectInst.getTrueValue(),
                                  *select.mutable_when_true());
    // False
    llvm2col::transformAndSetExpr(funcCursor, selectInst,
                                  *selectInst.getFalseValue(),
                                  *select.mutable_when_false());
}

void llvm2col::transformPhi(llvm::PHINode &phiInstruction,
                            col::LlvmBasicBlock &colBlock,
                            pallas::FunctionCursor &funcCursor) {
    col::Variable &varDecl = funcCursor.declareVariable(phiInstruction);
    for (auto &B : phiInstruction.blocks()) {
        // add assignment of the variable to the block of the conditional
        // branch
        col::LlvmBasicBlock &targetBlock =
            funcCursor.getOrSetLLVMBlock2ColBlockEntry(*B);

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
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(icmpInstruction, colBlock);
    switch (llvm::ICmpInst::Predicate(icmpInstruction.getPredicate())) {
    case llvm::CmpInst::ICMP_EQ: {
        col::AmbiguousEq &eq =
            *assignment.mutable_value()->mutable_ambiguous_eq();
        eq.mutable_vector_inner_type()->mutable_t_int()->set_allocated_origin(
            generateBinExprOrigin(icmpInstruction));
        transformCmpExpr(icmpInstruction, eq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_NE: {
        col::AmbiguousNeq &neq =
            *assignment.mutable_value()->mutable_ambiguous_neq();
        neq.mutable_vector_inner_type()->mutable_t_int()->set_allocated_origin(
            generateBinExprOrigin(icmpInstruction));
        transformCmpExpr(icmpInstruction, neq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SGT:
    case llvm::CmpInst::ICMP_UGT: {
        col::AmbiguousGreater &gt =
            *assignment.mutable_value()->mutable_ambiguous_greater();
        transformCmpExpr(icmpInstruction, gt, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SGE:
    case llvm::CmpInst::ICMP_UGE: {
        col::AmbiguousGreaterEq &geq =
            *assignment.mutable_value()->mutable_ambiguous_greater_eq();
        transformCmpExpr(icmpInstruction, geq, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SLT:
    case llvm::CmpInst::ICMP_ULT: {
        col::AmbiguousLess &lt =
            *assignment.mutable_value()->mutable_ambiguous_less();
        transformCmpExpr(icmpInstruction, lt, funcCursor);
        break;
    }
    case llvm::CmpInst::ICMP_SLE:
    case llvm::CmpInst::ICMP_ULE: {
        col::AmbiguousLessEq &leq =
            *assignment.mutable_value()->mutable_ambiguous_less_eq();
        transformCmpExpr(icmpInstruction, leq, funcCursor);
        break;
    }
    default:
        pallas::ErrorReporter::addError(SOURCE_LOC, "Unknown ICMP predicate",
                                        icmpInstruction);
    }
}

void llvm2col::transformFCmp(llvm::FCmpInst &fcmpInstruction,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // TODO: Deal with fastmath flags
    // TODO: Deal with NaNs, LLVM generally pretends signalling NaNs don't
    //       exist so we should probably also only worry about QNaNs but we
    //       don't support NaNs at all right now in VerCors anyway so all this
    //       doesn't matter yet.
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(fcmpInstruction, colBlock);
    switch (llvm::FCmpInst::Predicate(fcmpInstruction.getPredicate())) {
    // From the documentation:
    //    FCMP_FALSE = 0, ///< 0 0 0 0    Always false (always folded)
    //    FCMP_OEQ = 1,   ///< 0 0 0 1    True if ordered and equal
    //    FCMP_OGT = 2,   ///< 0 0 1 0    True if ordered and greater than
    //    FCMP_OGE = 3,   ///< 0 0 1 1    True if ordered and greater than or
    //    equal FCMP_OLT = 4,   ///< 0 1 0 0    True if ordered and less than
    //    FCMP_OLE = 5,   ///< 0 1 0 1    True if ordered and less than or equal
    //    FCMP_ONE = 6,   ///< 0 1 1 0    True if ordered and operands are
    //    unequal FCMP_ORD = 7,   ///< 0 1 1 1    True if ordered (no nans)
    //    FCMP_UNO = 8,   ///< 1 0 0 0    True if unordered: isnan(X) | isnan(Y)
    //    FCMP_UEQ = 9,   ///< 1 0 0 1    True if unordered or equal
    //    FCMP_UGT = 10,  ///< 1 0 1 0    True if unordered or greater than
    //    FCMP_UGE = 11,  ///< 1 0 1 1    True if unordered, greater than, or
    //    equal FCMP_ULT = 12,  ///< 1 1 0 0    True if unordered or less than
    //    FCMP_ULE = 13,  ///< 1 1 0 1    True if unordered, less than, or equal
    //    FCMP_UNE = 14,  ///< 1 1 1 0    True if unordered or not equal
    //    FCMP_TRUE = 15, ///< 1 1 1 1    Always true (always folded)
    case llvm::CmpInst::FCMP_FALSE: {
        col::BooleanValue &boolean =
            *assignment.mutable_value()->mutable_boolean_value();
        boolean.set_value(false);
        boolean.set_allocated_origin(generateBinExprOrigin(fcmpInstruction));
        break;
    }
    case llvm::CmpInst::FCMP_OEQ:
    case llvm::CmpInst::FCMP_UEQ: {
        col::AmbiguousEq &eq =
            *assignment.mutable_value()->mutable_ambiguous_eq();
        eq.mutable_vector_inner_type()->mutable_t_int()->set_allocated_origin(
            generateBinExprOrigin(fcmpInstruction));
        transformCmpExpr(fcmpInstruction, eq, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_OGT:
    case llvm::CmpInst::FCMP_UGT: {
        col::AmbiguousGreater &gt =
            *assignment.mutable_value()->mutable_ambiguous_greater();
        transformCmpExpr(fcmpInstruction, gt, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_OGE:
    case llvm::CmpInst::FCMP_UGE: {
        col::AmbiguousGreaterEq &geq =
            *assignment.mutable_value()->mutable_ambiguous_greater_eq();
        transformCmpExpr(fcmpInstruction, geq, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_OLT:
    case llvm::CmpInst::FCMP_ULT: {
        col::AmbiguousLess &lt =
            *assignment.mutable_value()->mutable_ambiguous_less();
        transformCmpExpr(fcmpInstruction, lt, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_OLE:
    case llvm::CmpInst::FCMP_ULE: {
        col::AmbiguousLessEq &leq =
            *assignment.mutable_value()->mutable_ambiguous_less_eq();
        transformCmpExpr(fcmpInstruction, leq, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_ONE:
    case llvm::CmpInst::FCMP_UNE: {
        col::AmbiguousNeq &neq =
            *assignment.mutable_value()->mutable_ambiguous_neq();
        neq.mutable_vector_inner_type()->mutable_t_int()->set_allocated_origin(
            generateBinExprOrigin(fcmpInstruction));
        transformCmpExpr(fcmpInstruction, neq, funcCursor);
        break;
    }
    case llvm::CmpInst::FCMP_TRUE: {
        col::BooleanValue &boolean =
            *assignment.mutable_value()->mutable_boolean_value();
        boolean.set_value(true);
        boolean.set_allocated_origin(generateBinExprOrigin(fcmpInstruction));
        break;
    }
    case llvm::CmpInst::FCMP_ORD:
    case llvm::CmpInst::FCMP_UNO: {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Checking for NaNs is unsupported", fcmpInstruction);
        break;
    }
    default:
        pallas::ErrorReporter::addError(SOURCE_LOC, "Unknown FCMP predicate",
                                        fcmpInstruction);
    }
}

void llvm2col::transformCmpExpr(llvm::CmpInst &cmpInstruction,
                                auto &colCompareExpr,
                                pallas::FunctionCursor &funcCursor) {
    transformBinExpr(cmpInstruction, colCompareExpr, funcCursor);
}

void llvm2col::transformExtractValueInst(
    llvm::ExtractValueInst &llvmInstruction, col::LlvmBasicBlock &colBlock,
    pallas::FunctionCursor &funcCursor) {
    auto *pFunc = llvmInstruction.getFunction();
    auto &mamProxy =
        funcCursor.getFunctionAnalysisManager()
            .getResult<llvm::ModuleAnalysisManagerFunctionProxy>(*pFunc);
    auto *sdRes =
        mamProxy.getCachedResult<pallas::StructTDeclarer>(*pFunc->getParent());
    assert(sdRes != nullptr);
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(llvmInstruction, colBlock);
    col::LlvmExtractValue *extrVal =
        assignment.mutable_value()->mutable_llvm_extract_value();
    extrVal->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmInstruction));
    extrVal->set_allocated_blame(new col::Blame{});
    // Aggregate type
    llvm2col::transformAndSetValueType(
        *llvmInstruction.getAggregateOperand(), nullptr,
        *extrVal->mutable_aggregate_type(), *sdRes);
    // Result type
    llvm2col::transformAndSetValueType(llvmInstruction, nullptr,
                                       *extrVal->mutable_result_type(), *sdRes);
    // Value
    llvm2col::transformAndSetExpr(funcCursor, llvmInstruction,
                                  *llvmInstruction.getAggregateOperand(),
                                  *extrVal->mutable_value());
    // Indices
    for (auto &index : llvmInstruction.indices()) {
        extrVal->add_indices(index);
    }
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
                                 col::LlvmBasicBlock &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    if (!checkCallSupport(callInstruction) ||
        callInstruction.getCalledFunction() == nullptr)
        return;

    if (callInstruction.getCalledFunction()->isIntrinsic()) {
        transformIntrinsic(callInstruction, colBlock, funcCursor);
        return;
    }

    // If it is a call to a function from the pallas specification library,
    // we transform it into the appropriate col-node.
    if (pallas::irspec::isPallasSpecLib(*callInstruction.getCalledFunction())) {
        transformPallasSpecLibCall(callInstruction, colBlock, funcCursor);
        return;
    }

    // allocate expression to host the function call in advance
    col::Expr *functionCallExpr;
    // if void function add an eval expression
    if (callInstruction.getType()->isVoidTy()) {
        col::Eval *eval =
            pallas::bodyAsBlock(colBlock).add_statements()->mutable_eval();
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

    // Given-bindings
    if (auto gBindingMD =
            pallas::irspec::getGivenBindingBlockMD(callInstruction)) {
        auto givenBlock = pallas::irspec::getGivenBindingBlock(gBindingMD);
        if (!givenBlock.has_value())
            return;
        auto *calledFunc = callInstruction.getCalledFunction();
        auto &calledContrRes = funcCursor.getFDCResult(*calledFunc);
        if (calledContrRes.getIRContract() == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Unable to get ghost args from  contract of called function",
                callInstruction);
            return;
        }

        for (auto &g : givenBlock->bindings) {
            auto *givenEntry = invocation->add_given_map();
            // Given-variable
            auto *colGivenVar =
                calledContrRes.getGhostArgMapEntry(*g.getGivenDef());
            auto *gVarRef = givenEntry->mutable_v1();
            gVarRef->set_id(colGivenVar->id());

            // Call to wrapper function
            auto *colWrapperCall =
                givenEntry->mutable_v2()->mutable_llvm_function_invocation();
            llvm2col::buildWrapperCall(
                g, callInstruction, *callInstruction.getFunction(),
                *colWrapperCall, funcCursor, stmntVarMapper);
        }
    }

    // Handle yields-bindings
    if (auto yBindingsMD =
            pallas::irspec::getYieldsBindingBlockMD(callInstruction)) {
        auto yieldsBlock = pallas::irspec::getYieldsBindingBlock(yBindingsMD);
        if (!yieldsBlock.has_value())
            return;

        // Get contract of called function
        auto &calledContrRes =
            funcCursor.getFDCResult(*callInstruction.getCalledFunction());
        if (calledContrRes.getIRContract() == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to get contract of called function",
                callInstruction);
            return;
        }

        // Get contract of parent function
        auto &parentContrRes =
            funcCursor.getFDCResult(*callInstruction.getParent()->getParent());
        if (parentContrRes.getIRContract() == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to get contract of parent function",
                callInstruction);
            return;
        }

        for (auto &y : yieldsBlock->bindings) {
            auto *yieldsEntry = invocation->add_yields();

            // Expr (Ghost var from parent function)
            auto *targetVar =
                parentContrRes.getGhostArgMapEntry(y.getTargetVar());
            if (targetVar == nullptr) {
                pallas::ErrorReporter::addError(
                    SOURCE_LOC, "Unable to get ghost var from parent function",
                    callInstruction);
                return;
            }
            auto targetName =
                pallas::irspec::getGhostArgDef(&y.getTargetVar())->name;
            auto *targetLoc = yieldsEntry->mutable_v1()->mutable_local();
            targetLoc->set_allocated_origin(
                llvm2col::generatePallasSpecOrigin(y.getLoc(), targetName));
            targetLoc->mutable_ref()->set_id(targetVar->id());

            // Yields var from called function
            auto *yieldsVar =
                calledContrRes.getGhostArgMapEntry(y.getYieldsArg());
            if (targetVar == nullptr) {
                pallas::ErrorReporter::addError(
                    SOURCE_LOC, "Unable to get yields arg from called function",
                    callInstruction);
                return;
            }
            yieldsEntry->mutable_v2()->set_id(yieldsVar->id());
        }
    }
}

void llvm2col::transformPallasSpecLibCall(llvm::CallInst &callInstruction,
                                          col::LlvmBasicBlock &colBlock,
                                          pallas::FunctionCursor &funcCursor) {
    auto specLibType =
        pallas::irspec::isPallasSpecLib(*callInstruction.getCalledFunction())
            .value();

    if (specLibType == pallas::constants::PALLAS_SPEC_RESULT) {
        transformPallasSpecResult(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_FRAC_OF) {
        transformPallasFracOf(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_PERM) {
        transformPallasPerm(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_PTR_BLOCK_LENGTH) {
        transformPallasPtrBlockLength(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_PTR_BLOCK_OFFSET) {
        transformPallasPtrBlockOffset(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_PTR_LENGTH) {
        transformPallasPtrLength(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_IMPLY) {
        transformPallasImply(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_AND) {
        transformPallasAnd(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_OR) {
        transformPallasOr(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_STAR) {
        transformPallasStar(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_OLD) {
        transformPallasOld(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_BV) {
        transformPallasBoundVar(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_FORALL) {
        transformPallasForall(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEPFORALL) {
        transformPallasSepForall(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_EXISTS) {
        transformPallasExists(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_UNFOLDING) {
        transformPallasUnfolding(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEQ_NEW) {
        transformPallasSeqNew(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEQ_SIZE) {
        transformPallasSeqSize(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEQ_EQUALS) {
        transformPallasSeqEq(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEQ_GET) {
        transformPallasSeqGet(callInstruction, colBlock, funcCursor);
    } else if (specLibType == pallas::constants::PALLAS_SPEC_SEQ_SLICE) {
        transformPallasSeqSlice(callInstruction, colBlock, funcCursor);
    } else {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unsupported Pallas specification function ",
            callInstruction);
    }
}

namespace {
bool checkQuantifierSpecFuncWellformed(llvm::Function &specFunc,
                                       const std::string &errorDesc) {
    if (specFunc.arg_size() == 2 &&
        specFunc.getArg(0)->getType()->isIntegerTy(1) &&
        specFunc.getArg(0)->getType()->isIntegerTy(1) &&
        specFunc.getReturnType()->isIntegerTy(1)) {
        return true;
    }
    pallas::ErrorReporter::addError(
        SOURCE_LOC, "Malformed pallas spec-lib definition (" + errorDesc + ").",
        specFunc);
    return false;
}

// Checks if the definition of a given given specification-function
// is a function that takes two booleans and returns a boolean.
bool checkBinaryBoolOpWellformed(llvm::Function &specFunc,
                                 const std::string &errorDesc) {
    if (specFunc.arg_size() == 2 &&
        specFunc.getArg(0)->getType()->isIntegerTy(1) &&
        specFunc.getArg(1)->getType()->isIntegerTy(1) &&
        specFunc.getReturnType()->isIntegerTy(1)) {
        return true;
    }
    pallas::ErrorReporter::addError(
        SOURCE_LOC, "Malformed pallas spec-lib definition (" + errorDesc + ").",
        specFunc);
    return false;
}

// Checks if the definition of a pointer-block spec function is wellformed.
// (I.e. it is a function that takes a pointer and returns an i64.
bool checkPtrBlockSpecFuncWellformed(llvm::Function &specFunc,
                                     const std::string &errorDesc) {
    if (specFunc.arg_size() == 1 &&
        specFunc.getArg(0)->getType()->isPointerTy() &&
        specFunc.getReturnType()->isIntegerTy(64)) {
        return true;
    }
    pallas::ErrorReporter::addError(
        SOURCE_LOC, "Malformed pallas spec-lib definition (" + errorDesc + ").",
        specFunc);
    return false;
}

} // namespace

void llvm2col::transformPallasSpecResult(llvm::CallInst &callInstruction,
                                         col::LlvmBasicBlock &colBlock,
                                         pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    bool isRegularReturn = !llvmSpecFunc->getReturnType()->isVoidTy();

    // Get the function to whose contract this call instuction belongs to.
    auto *wrapperFunc = callInstruction.getFunction();
    auto *llvmParentFunc =
        funcCursor.getFunctionAnalysisManager()
            .getResult<pallas::ExprWrapperMapper>(*wrapperFunc)
            .getParentFunc();
    if (llvmParentFunc == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Encountered call to spec-lib that cannot be associated "
            "with a function",
            callInstruction);
        return;
    }
    auto &colParentFunc = funcCursor.getFDResult(*llvmParentFunc);

    if (isRegularReturn) {
        // Case 1: Result is returned as regular return-value
        // %2 = call i32 @pallas.result.0()

        // Check that the function signature is wellformed
        if (!llvmSpecFunc->arg_empty()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Malformed pallas spec-lib result-function. Expected no "
                "arguments.",
                callInstruction);
            return;
        }

        // Build the assignment-expression
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        auto *assignExpr = assignment.mutable_value();
        auto *resultNode = assignExpr->mutable_llvm_result();
        resultNode->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        // Set ref to the function to which this contract is attached to
        resultNode->mutable_func()->set_id(colParentFunc.getFunctionId());

    } else {
        // Case 2: Result is returned as a sret parameter
        if (llvmSpecFunc->arg_size() != 1 ||
            !llvmSpecFunc->getArg(0)->hasStructRetAttr()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Malformed pallas spec-lib result-function. Expected one "
                "sret-argument.",
                callInstruction);
            return;
        }

        // Replace the call to the result-function with a store-instruction that
        // stores the value of \result.
        col::LlvmStore *store = pallas::bodyAsBlock(colBlock)
                                    .add_statements()
                                    ->mutable_llvm_store();
        store->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        store->set_allocated_blame(new col::Blame());
        // Value
        col::LlvmResult *value = store->mutable_value()->mutable_llvm_result();
        value->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        value->mutable_func()->set_id(colParentFunc.getFunctionId());
        // Pointer
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(0),
                                      *store->mutable_pointer());
        // Memory ordering (Set to sequentially consistent)
        col::LlvmMemorySequentiallyConsistent *memOrder =
            store->mutable_ordering()
                ->mutable_llvm_memory_sequentially_consistent();
        memOrder->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
    }
}

void llvm2col::transformPallasFracOf(llvm::CallInst &callInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();

    //  Check that the signature matches
    if (llvmSpecFunc->arg_size() != 3 ||
        !llvmSpecFunc->getArg(0)->hasStructRetAttr() ||
        !llvmSpecFunc->getArg(1)->getType()->isIntegerTy() ||
        !llvmSpecFunc->getArg(2)->getType()->isIntegerTy()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib fracOf-function.",
            callInstruction);
        return;
    }

    // Check that the value of the sret-argument is an alloca
    auto *sretAlloc = llvm::dyn_cast_if_present<llvm::AllocaInst>(
        callInstruction.getArgOperand(0));
    if (sretAlloc == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Malformed call to fracOf. First argument should be alloca",
            callInstruction);
        return;
    }

    col::LlvmFracOf *fracOf =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_frac_of();
    fracOf->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    fracOf->set_allocated_blame(new col::Blame());

    fracOf->mutable_sret()->set_id(
        funcCursor.getVariableMapEntry(*sretAlloc, false).id());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *fracOf->mutable_num());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(2),
                                  *fracOf->mutable_denom());
}

void llvm2col::transformPallasPerm(llvm::CallInst &callInstruction,
                                   col::LlvmBasicBlock &colBlock,
                                   pallas::FunctionCursor &funcCursor) {
    // Check that the function signature is wellformed
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (llvmSpecFunc->arg_size() != 2 ||
        !llvmSpecFunc->getArg(0)->getType()->isPointerTy() ||
        !llvmSpecFunc->getArg(1)->getType()->isPointerTy() ||
        !llvmSpecFunc->getArg(1)->hasByValAttr()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib definition (Perm).",
            callInstruction);
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *perm = assignment.mutable_value()->mutable_llvm_perm();
    perm->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    perm->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *perm->mutable_loc());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *perm->mutable_perm());
}

void llvm2col::transformPallasPtrBlockLength(
    llvm::CallInst &callInstruction, col::LlvmBasicBlock &colBlock,
    pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkPtrBlockSpecFuncWellformed(*llvmSpecFunc, "PtrBlockLength")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *pbl = assignment.mutable_value()->mutable_llvm_ptr_block_length();
    pbl->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    pbl->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *pbl->mutable_ptr());
}

void llvm2col::transformPallasPtrBlockOffset(
    llvm::CallInst &callInstruction, col::LlvmBasicBlock &colBlock,
    pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkPtrBlockSpecFuncWellformed(*llvmSpecFunc, "PtrBlockOffset")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *pbo = assignment.mutable_value()->mutable_llvm_ptr_block_offset();
    pbo->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    pbo->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *pbo->mutable_ptr());
}

void llvm2col::transformPallasPtrLength(llvm::CallInst &callInstruction,
                                        col::LlvmBasicBlock &colBlock,
                                        pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkPtrBlockSpecFuncWellformed(*llvmSpecFunc, "PtrLength")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *pl = assignment.mutable_value()->mutable_llvm_ptr_length();
    pl->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    pl->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *pl->mutable_ptr());
}

void llvm2col::transformPallasImply(llvm::CallInst &callInstruction,
                                    col::LlvmBasicBlock &colBlock,
                                    pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    // Check that the function signature is wellformed
    if (!checkBinaryBoolOpWellformed(*llvmSpecFunc, "Imply")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *imply = assignment.mutable_value()->mutable_llvm_implies();
    imply->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *imply->mutable_left());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *imply->mutable_right());
}

void llvm2col::transformPallasAnd(llvm::CallInst &callInstruction,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    // Check that the function signature is wellformed
    if (!checkBinaryBoolOpWellformed(*llvmSpecFunc, "And")) {
        return;
    }
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *imply = assignment.mutable_value()->mutable_llvm_and();
    imply->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *imply->mutable_left());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *imply->mutable_right());
}

void llvm2col::transformPallasOr(llvm::CallInst &callInstruction,
                                 col::LlvmBasicBlock &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    // Check that the function signature is wellformed
    if (!checkBinaryBoolOpWellformed(*llvmSpecFunc, "Or")) {
        return;
    }
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *imply = assignment.mutable_value()->mutable_llvm_or();
    imply->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *imply->mutable_left());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *imply->mutable_right());
}

void llvm2col::transformPallasStar(llvm::CallInst &callInstruction,
                                   col::LlvmBasicBlock &colBlock,

                                   pallas::FunctionCursor &funcCursor) {
    // Check that the function signature is wellformed
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkBinaryBoolOpWellformed(*llvmSpecFunc, "**")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *star = assignment.mutable_value()->mutable_llvm_star();
    star->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *star->mutable_left());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *star->mutable_right());
}

void llvm2col::transformPallasOld(llvm::CallInst &callInstruction,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    bool isRegularReturn = !llvmSpecFunc->getReturnType()->isVoidTy();
    bool isRegularPass =
        llvmSpecFunc->arg_size() == 1 &&
        !llvmSpecFunc->getArg(0)->hasByValAttr() &&
        (llvmSpecFunc->getArg(0)->getType() == llvmSpecFunc->getReturnType());

    // "Normal" return and pass of value.
    if (isRegularReturn && isRegularPass) {
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        auto *old = assignment.mutable_value()->mutable_llvm_old();
        old->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(0),
                                      *old->mutable_v());
    } else {
        pallas::ErrorReporter::addError(SOURCE_LOC, "Unsupported use of \\old.",
                                        callInstruction);
        return;
    }

    // TODO: Handle other cases (big structs, small structs, ...)
}

void llvm2col::transformPallasBoundVar(llvm::CallInst &callInstruction,
                                       col::LlvmBasicBlock &colBlock,
                                       pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    bool isRegularReturn = !llvmSpecFunc->getReturnType()->isVoidTy();

    // "Normal" return.
    if (llvmSpecFunc->arg_size() == 1 && isRegularReturn) {
        auto &sdRes = getSDResult(funcCursor, callInstruction);
        auto *type = llvmSpecFunc->getReturnType();
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        auto *bv = assignment.mutable_value()->mutable_llvm_bound_var();
        bv->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        // Extract the string-literal that is used as the
        //  identifier of the bound variable.
        auto *idVar = llvm::dyn_cast<llvm::GlobalVariable>(
            callInstruction.getArgOperand(0));
        if (idVar == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Invalid identifier (BoundVar)", callInstruction);
            return;
        }
        auto *constArr = llvm::dyn_cast_if_present<llvm::ConstantDataArray>(
            idVar->getInitializer());
        if (constArr == nullptr || !constArr->isString()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Invalid identifier (BoundVar)", callInstruction);
            return;
        }
        auto strRepr = constArr->isCString() ? constArr->getAsCString()
                                             : constArr->getAsString();
        bv->set_id(strRepr.str());
        if (auto *subProgram = llvmSpecFunc->getSubprogram()) {
            auto diType = llvm::dyn_cast<llvm::DIType>(
                subProgram->getType()->getTypeArray()->getOperand(0));
            llvm2col::transformAndSetTypeWithDebugInfo(
                llvmSpecFunc->getReturnType(), diType, *bv->mutable_var_type(),
                sdRes);
        } else {
            llvm2col::transformAndSetType(*type, *bv->mutable_var_type(),
                                          sdRes);
        }
    } else {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unsupported use of bound variable.", callInstruction);
        return;
    }
    // TODO: Handle other cases
}

void llvm2col::transformPallasForall(llvm::CallInst &callInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkQuantifierSpecFuncWellformed(*llvmSpecFunc, "forall")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *quantifier = assignment.mutable_value()->mutable_llvm_forall();
    quantifier->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *quantifier->mutable_binding_expr());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *quantifier->mutable_body_expr());
}

void llvm2col::transformPallasSepForall(llvm::CallInst &callInstruction,
                                        col::LlvmBasicBlock &colBlock,
                                        pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkQuantifierSpecFuncWellformed(*llvmSpecFunc, "forall*")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *quantifier = assignment.mutable_value()->mutable_llvm_sep_forall();
    quantifier->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    quantifier->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *quantifier->mutable_binding_expr());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *quantifier->mutable_body_expr());
}

void llvm2col::transformPallasExists(llvm::CallInst &callInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (!checkQuantifierSpecFuncWellformed(*llvmSpecFunc, "exists")) {
        return;
    }

    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *quantifier = assignment.mutable_value()->mutable_llvm_exists();
    quantifier->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *quantifier->mutable_binding_expr());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *quantifier->mutable_body_expr());
}

void llvm2col::transformPallasUnfolding(llvm::CallInst &callInstruction,
                                        col::LlvmBasicBlock &colBlock,
                                        pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    bool isRegularReturn = !llvmSpecFunc->getReturnType()->isVoidTy();
    bool isRegularPass =
        llvmSpecFunc->arg_size() == 2 &&
        !llvmSpecFunc->getArg(1)->hasByValAttr() &&
        (llvmSpecFunc->getArg(1)->getType() == llvmSpecFunc->getReturnType());
    bool isBoolPred = llvmSpecFunc->arg_size() > 1 &&
                      llvmSpecFunc->getArg(0)->getType()->isIntegerTy(1);

    // "Normal" return and pass of value.
    if (isRegularReturn && isRegularPass && isBoolPred) {
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        auto *unfolding = assignment.mutable_value()->mutable_unfolding();
        unfolding->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        unfolding->set_allocated_blame(new col::Blame());
        auto *target =
            unfolding->mutable_res()->mutable_ambiguous_fold_target();
        target->set_allocated_origin(llvm2col::generateOperandOrigin(
            callInstruction, *callInstruction.getArgOperand(0)));
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(0),
                                      *target->mutable_target());
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(1),
                                      *unfolding->mutable_body());
    } else {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unsupported use of _unfolding.", callInstruction);
        return;
    }

    // TODO: Handle other cases (big structs, small structs, ...)
}

void llvm2col::transformPallasSeqNew(llvm::CallInst &callInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();

    //  Check that the signature matches
    if (llvmSpecFunc->arg_size() != 1 ||
        !llvmSpecFunc->getArg(0)->hasStructRetAttr()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib function: seq.new",
            callInstruction);
        return;
    }

    // Find the variable-id
    col::Variable *targetVar = nullptr;
    if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(
            callInstruction.getArgOperand(0))) {
        targetVar = &funcCursor.getVariableMapEntry(*alloca, false);
    } else if (auto *arg = llvm::dyn_cast<llvm::Argument>(
                   callInstruction.getArgOperand(0))) {
        targetVar = &funcCursor.getFDResult(*callInstruction.getFunction())
                         .getFuncArgMapEntry(*arg);
    }

    if (targetVar == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Failed to get target variable for seq.new",
            callInstruction);
        return;
    }

    // Get the content-type from the sequence
    auto *llvmSeqT = callInstruction.getParamStructRetType(0);
    auto *llvmSeqContentT = getPallasSequenceContentType(llvmSeqT);
    if (llvmSeqContentT == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Failed to get content type of sequence while transforming seq.new",
            callInstruction);
        return;
    }

    auto &sdRes = getSDResult(funcCursor, callInstruction);
    auto *colSeqNew =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_seq_new();
    colSeqNew->set_allocated_blame(new col::Blame());
    colSeqNew->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(callInstruction));
    llvm2col::transformAndSetType(*llvmSeqContentT,
                                  *colSeqNew->mutable_c_type(), sdRes);
    auto *colTargetDeref = colSeqNew->mutable_target()->mutable_deref_pointer();
    colTargetDeref->set_allocated_blame(new col::Blame());
    colTargetDeref->set_allocated_origin(llvm2col::generateOperandOrigin(
        callInstruction, *callInstruction.getArgOperand(0)));
    auto *colTarget = colTargetDeref->mutable_pointer()->mutable_local();
    colTarget->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(callInstruction));
    colTarget->mutable_ref()->set_id(targetVar->id());
}

void llvm2col::transformPallasSeqSize(llvm::CallInst &callInstruction,
                                      col::LlvmBasicBlock &colBlock,
                                      pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (llvmSpecFunc->arg_size() != 1 ||
        !llvmSpecFunc->getReturnType()->isIntegerTy()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib function: seq.size",
            callInstruction);
        return;
    }

    auto &assign =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *colSize = assign.mutable_value()->mutable_llvm_seq_size();
    colSize->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    colSize->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *colSize->mutable_seq());
}

void llvm2col::transformPallasSeqEq(llvm::CallInst &callInstruction,
                                    col::LlvmBasicBlock &colBlock,
                                    pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();

    // Check function signature
    if (llvmSpecFunc->arg_size() != 2 ||
        !llvmSpecFunc->getReturnType()->isIntegerTy(1)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib function: seq.eq",
            callInstruction);
        return;
    }
    auto &assign =
        funcCursor.createAssignmentAndDeclaration(callInstruction, colBlock);
    auto *colEq = assign.mutable_value()->mutable_llvm_seq_eq();
    colEq->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    colEq->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *colEq->mutable_s1());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *colEq->mutable_s2());
}

void llvm2col::transformPallasSeqGet(llvm::CallInst &callInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    bool isRegularReturn = !llvmSpecFunc->getReturnType()->isVoidTy();
    auto &sdRes = getSDResult(funcCursor, callInstruction);

    auto *llvmSeqT = isRegularReturn
                         ? llvmSpecFunc->getArg(0)->getParamByValType()
                         : llvmSpecFunc->getArg(0)->getParamByValType();
    auto *llvmSeqContentT = llvm2col::getPallasSequenceContentType(llvmSeqT);
    if (llvmSeqContentT == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Failed to get content-type of sequence (seq.get)",
            callInstruction);
        return;
    }

    if (isRegularReturn) {
        // Case 1: Result is returned as regular return-value

        // Check that the function signature is wellformed
        if (llvmSpecFunc->arg_size() != 2 ||
            !llvmSpecFunc->getArg(0)->getType()->isPointerTy() ||
            !llvmSpecFunc->getArg(0)->hasByValAttr() ||
            !llvmSpecFunc->getArg(1)->getType()->isIntegerTy()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Malformed pallas spec-lib function: seq.get",
                callInstruction);
            return;
        }

        // Build the assignment-expression
        auto &assign = funcCursor.createAssignmentAndDeclaration(
            callInstruction, colBlock);
        auto *seqGet = assign.mutable_value()->mutable_llvm_seq_get();
        seqGet->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        seqGet->set_allocated_blame(new col::Blame());
        llvm2col::transformAndSetType(*llvmSeqContentT,
                                      *seqGet->mutable_elem_type(), sdRes);
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(0),
                                      *seqGet->mutable_seq());
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(1),
                                      *seqGet->mutable_idx());
    } else {
        // Case 2: Result is returned as a sret parameter
        if (llvmSpecFunc->arg_size() != 3 ||
            !llvmSpecFunc->getArg(0)->getType()->isPointerTy() ||
            !llvmSpecFunc->getArg(0)->hasStructRetAttr() ||
            !llvmSpecFunc->getArg(1)->getType()->isPointerTy() ||
            !llvmSpecFunc->getArg(1)->hasByValAttr() ||
            !llvmSpecFunc->getArg(2)->getType()->isIntegerTy()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Malformed pallas spec-lib function: seq.get",
                callInstruction);
            return;
        }

        // Build store-instruction instead of assignment
        auto *store = pallas::bodyAsBlock(colBlock)
                          .add_statements()
                          ->mutable_llvm_store();
        store->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        store->set_allocated_blame(new col::Blame());
        // Value
        auto *seqGet = store->mutable_value()->mutable_llvm_seq_get();
        seqGet->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
        seqGet->set_allocated_blame(new col::Blame());
        llvm2col::transformAndSetType(*llvmSeqContentT,
                                      *seqGet->mutable_elem_type(), sdRes);
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(1),
                                      *seqGet->mutable_seq());
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(2),
                                      *seqGet->mutable_idx());
        // Target
        llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                      *callInstruction.getArgOperand(0),
                                      *store->mutable_pointer());
        // Memory ordering (Set to sequentially consistent)
        col::LlvmMemorySequentiallyConsistent *memOrder =
            store->mutable_ordering()
                ->mutable_llvm_memory_sequentially_consistent();
        memOrder->set_allocated_origin(
            llvm2col::generateFunctionCallOrigin(callInstruction));
    }
}

void llvm2col::transformPallasSeqSlice(llvm::CallInst &callInstruction,
                                       col::LlvmBasicBlock &colBlock,
                                       pallas::FunctionCursor &funcCursor) {
    auto *llvmSpecFunc = callInstruction.getCalledFunction();
    if (llvmSpecFunc->arg_size() != 4 ||
        !llvmSpecFunc->getArg(0)->hasStructRetAttr() ||
        !llvmSpecFunc->getArg(0)->getType()->isPointerTy() ||
        !llvmSpecFunc->getArg(1)->getType()->isPointerTy() ||
        !llvmSpecFunc->getArg(2)->getType()->isIntegerTy() ||
        !llvmSpecFunc->getArg(3)->getType()->isIntegerTy()) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed pallas spec-lib function: seq.slice",
            callInstruction);
        return;
    }

    // Build store-instruction because result is returned as sret
    auto *store =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_store();
    store->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    store->set_allocated_blame(new col::Blame());
    // Value
    auto *seqSlice = store->mutable_value()->mutable_llvm_seq_slice();
    seqSlice->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
    seqSlice->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(1),
                                  *seqSlice->mutable_seq());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(2),
                                  *seqSlice->mutable_s_idx());
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(3),
                                  *seqSlice->mutable_e_idx());
    // Target
    llvm2col::transformAndSetExpr(funcCursor, callInstruction,
                                  *callInstruction.getArgOperand(0),
                                  *store->mutable_pointer());
    // Memory ordering (Set to sequentially consistent)
    col::LlvmMemorySequentiallyConsistent *memOrder =
        store->mutable_ordering()
            ->mutable_llvm_memory_sequentially_consistent();
    memOrder->set_allocated_origin(
        llvm2col::generateFunctionCallOrigin(callInstruction));
}