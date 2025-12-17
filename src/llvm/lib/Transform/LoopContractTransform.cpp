#include "Transform/LoopContractTransform.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Transform/Transform.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasMD.h"
#include "Util/PallasWrapperUtils.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/raw_ostream.h>
#include <string>

const std::string SOURCE_LOC = "Transform::LoopContractTransform";

namespace {
void addError(llvm::Function &parentFunc, const std::string &msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC, msg, parentFunc);
}
} // namespace

void llvm2col::transformLoopContract(llvm::Loop &llvmLoop,
                                     col::LoopContract &colContract,
                                     pallas::FunctionCursor &functionCursor) {
    llvm::MDNode *contractMD = pallas::utils::getPallasLoopContract(llvmLoop);
    if (contractMD == nullptr) {
        initializeEmptyLoopContract(colContract);
        return;
    }

    // Get the source-location from the contract
    llvm::MDNode *contractSrcLoc =
        llvm::dyn_cast<llvm::MDNode>(contractMD->getOperand(1).get());
    if (contractSrcLoc == nullptr ||
        !pallas::utils::isWellformedPallasLocation(contractSrcLoc)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Malformed loop contract. Expected src-location as second operand.",
            *llvmLoop.getHeader()->getParent());
        return;
    }

    col::LlvmLoopContract *colInvariant =
        colContract.mutable_llvm_loop_contract();
    colInvariant->set_allocated_origin(
        generatePallasLoopContractOrigin(llvmLoop, *contractSrcLoc));
    colInvariant->mutable_blame();

    // Check that the loop-contract contains invariants.
    if (contractMD->getNumOperands() < 3) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Malformed loop contract. No invariants were provided.",
            *llvmLoop.getHeader()->getParent());
        return;
    }

    // Extract invariants and add them to the contract
    unsigned int opIdx = 2;
    while (opIdx < contractMD->getNumOperands()) {
        // Cast operand into MDNode
        llvm::MDNode *invMD = llvm::dyn_cast_if_present<llvm::MDNode>(
            contractMD->getOperand(opIdx).get());
        if (invMD == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Malformed loop-contract. Expected MDNode as operand.",
                *llvmLoop.getHeader()->getParent());
            return;
        }
        // Add invariant to contract
        if (!addInvariantToContract(*invMD, llvmLoop, *colInvariant,
                                    *contractSrcLoc, functionCursor)) {
            return;
        }
        ++opIdx;
    }
    return;
}

namespace {
llvm::DbgValueInst *selectDbgValue(
    const llvm::SmallVector<llvm::DbgVariableIntrinsic *> &intrinsics,
    llvm::Loop &llvmLoop, llvm::FunctionAnalysisManager &fam) {
    // Cast all intrinsics to dbg.value
    llvm::SmallVector<llvm::DbgValueInst *> dbgValues;
    for (auto *intr : intrinsics) {
        if (pallas::utils::hasDiExpression(*intr)) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable (DIExpressions not "
                            "yet supported)");
            return nullptr;
        }
        auto *dbgVal = llvm::dyn_cast<llvm::DbgValueInst>(intr);
        if (dbgVal == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable (Expected dbg.value)");
            return nullptr;
        }
        dbgValues.push_back(dbgVal);
    }
    // If there is a unique intrinsic, return that
    if (dbgValues.size() == 1) {
        return dbgValues.front();
    }

    // Try to find dbg.value that refers to phi-node in loop-header
    // (Applies to values that are modified within the loop)
    auto *loopHeader = llvmLoop.getHeader();
    llvm::DbgValueInst *valInHeader = nullptr;
    for (auto *dbgValue : dbgValues) {
        auto *phi = llvm::dyn_cast<llvm::PHINode>(dbgValue->getValue());
        // Only consider dbg-intrinsics in the loop-header that refer to a
        // phi-node in the loop-header.
        if (dbgValue->getParent() != loopHeader || phi == nullptr ||
            phi->getParent() != loopHeader) {
            continue;
        }
        if (valInHeader != nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable (Ambiguous dbg.value in "
                            "loop-header)");
            return nullptr;
        }
        valInHeader = dbgValue;
    }
    if (valInHeader != nullptr) {
        return valInHeader;
    }

    // Map to the next dbg.value intrinsic that preceeds the loop-header.
    // (Applies to values that are not modified in the loop)
    auto *closestDbgValue =
        pallas::utils::getClosestDbgValue(intrinsics, loopHeader->front(), fam);
    return closestDbgValue;
}

} // namespace

bool llvm2col::addInvariantToContract(llvm::MDNode &invMD, llvm::Loop &llvmLoop,
                                      col::LlvmLoopContract &colContract,
                                      llvm::MDNode &contractLoc,
                                      pallas::FunctionCursor &functionCursor) {
    pallas::FunctionAnalysisManager &fam =
        functionCursor.getFunctionAnalysisManager();
    llvm::Function *llvmParentFunc = llvmLoop.getHeader()->getParent();

    // Check wellformedness of MD-Node
    if (invMD.getNumOperands() < 2) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Malformed loop-invariant. Expected at least two operands.",
            *llvmParentFunc);
        return false;
    }

    // Extract src-location
    llvm::MDNode *srcLoc =
        llvm::dyn_cast_if_present<llvm::MDNode>(invMD.getOperand(0).get());
    if (srcLoc == nullptr ||
        !pallas::utils::isWellformedPallasLocation(srcLoc)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Malformed loop-invariant. Expected src-location as first operand.",
            *llvmParentFunc);
        return false;
    }

    // Get wrapper-function
    auto *llvmWFunc = pallas::utils::getWrapperFromLoopInv(invMD);
    if (llvmWFunc == nullptr) {
        pallas::ErrorReporter::addError(SOURCE_LOC,
                                        "Malformed loop-invariant. Expected "
                                        "wrapper-function as second operand.",
                                        *llvmParentFunc);
        return false;
    }
    col::LlvmFunctionDefinition &colWFunc =
        fam.getResult<pallas::FunctionDeclarer>(*llvmWFunc)
            .getAssociatedColFuncDef();

    // Get DIVariables from MD
    llvm::SmallVector<llvm::DILocalVariable *, 8> diVars;
    unsigned int idx = 2;
    while (idx < invMD.getNumOperands()) {
        // Check that operand is a DILocalVariable
        auto *diVar = llvm::dyn_cast_if_present<llvm::DILocalVariable>(
            invMD.getOperand(idx).get());
        if (diVar == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Malformed loop invariant. Expected DILocalVariable.",
                *llvmParentFunc);
            return false;
        }
        diVars.push_back(diVar);
        idx++;
    }

    pallas::FDResult &colFResult =
        fam.getResult<pallas::FunctionDeclarer>(*llvmParentFunc);
    col::LlvmFunctionDefinition &colParentFunc =
        colFResult.getAssociatedColFuncDef();

    // Build call to wrapper-function
    auto *wrapperCall = new col::LlvmFunctionInvocation();
    wrapperCall->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
    wrapperCall->set_allocated_blame(new col::Blame());
    wrapperCall->mutable_ref()->set_id(colWFunc.id());

    // Add arguments to wrapper-call
    for (auto [argIdx, diVar] : llvm::enumerate(diVars)) {
        llvm::SmallVector<llvm::DbgVariableIntrinsic *> intrinsics =
            pallas::utils::getIntrinsicsForDIVar(*llvmParentFunc, *diVar);

        if (auto *declIntr = pallas::utils::getUniqueDbgDeclare(intrinsics)) {
            // Map to unique dbg.declare
            if (pallas::utils::hasDiExpression(*declIntr)) {
                pallas::ErrorReporter::addError(
                    SOURCE_LOC, "Unable to map DIVariable (DIExpressions not "
                                "yet supported)");
                return false;
            }
            auto *alloca =
                llvm::dyn_cast<llvm::AllocaInst>(declIntr->getAddress());
            if (alloca == nullptr) {
                addError(*llvmParentFunc,
                         "Unable to map dbg.declare to instruction");
                return false;
            }
            pallas::utils::buildArgExprFromAlloca(*wrapperCall, argIdx, *alloca,
                                                  *llvmWFunc, *srcLoc,
                                                  functionCursor);
        } else {
            // Try to map to dbg.value in loop-header
            llvm::DbgValueInst *dbgVal =
                selectDbgValue(intrinsics, llvmLoop, fam);
            if (dbgVal == nullptr) {
                addError(*llvmParentFunc,
                         "Unable to map dbg.value to instruction.");
                return false;
            }
            bool ok = pallas::utils::buildArgExprFromDbgValue(
                *wrapperCall, argIdx, *dbgVal, *llvmWFunc, *srcLoc,
                functionCursor, *llvmParentFunc);
            if (!ok) {
                addError(*llvmParentFunc,
                         "Unable to build argument of wrapper-function.");
                return false;
            }
        }
    }

    // Append wrapper-call to loop-contract
    if (colContract.has_invariant()) {
        auto *oldInv = colContract.release_invariant();
        auto *newInv = colContract.mutable_invariant()->mutable_star();
        newInv->set_allocated_origin(
            generatePallasLoopContractOrigin(llvmLoop, contractLoc));
        newInv->set_allocated_left(oldInv);
        newInv->mutable_right()->set_allocated_llvm_function_invocation(
            wrapperCall);
    } else {
        colContract.mutable_invariant()->set_allocated_llvm_function_invocation(
            wrapperCall);
    }
    return true;
}

void llvm2col::initializeEmptyLoopContract(col::LoopContract &colContract) {
    col::LlvmLoopContract *invariant = colContract.mutable_llvm_loop_contract();
    col::BooleanValue *tt =
        invariant->mutable_invariant()->mutable_boolean_value();
    tt->set_value(true);
    tt->set_allocated_origin(generateLabelledOrigin("constant true"));
    invariant->set_allocated_origin(generateLabelledOrigin("constant true"));
    invariant->mutable_blame();
}