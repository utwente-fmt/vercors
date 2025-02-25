#include "Transform/LoopContractTransform.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasMD.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <string>

const std::string SOURCE_LOC = "Transform::LoopContractTransform";

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
    llvm::SmallVector<llvm::DIVariable *, 8> diVars;
    unsigned int idx = 2;
    while (idx < invMD.getNumOperands()) {
        // Check that operand is a DIVariable
        auto *diVar = llvm::dyn_cast_if_present<llvm::DIVariable>(
            invMD.getOperand(idx).get());
        if (diVar == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Malformed loop invariant. Expected DIVariable as operand.",
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
    for (auto *diVar : diVars) {
        // Match DIVariables to LLVM-Values
        llvm::Value *llvmVal =
            pallas::utils::mapDIVarToValue(*llvmParentFunc, *diVar, &llvmLoop);
        if (llvmVal == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable to value.",
                *llvmParentFunc);
            return false;
        }

        // Get variables from llvm-values and build argument-expressions
        if (llvm::isa<llvm::AllocaInst>(llvmVal)) {
            col::Variable *colVar =
                &functionCursor.getVariableMapEntry(*llvmVal, false);
            auto *ptrDeref = wrapperCall->add_args()->mutable_deref_pointer();
            ptrDeref->set_allocated_origin(
                llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
            ptrDeref->set_allocated_blame(new col::Blame());
            // Local to var of alloca
            auto *local = ptrDeref->mutable_pointer()->mutable_local();
            local->set_allocated_origin(
                llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
            local->mutable_ref()->set_id(colVar->id());
        } else if (llvm::isa<llvm::PHINode>(llvmVal)) {
            col::Variable *colVar =
                &functionCursor.getVariableMapEntry(*llvmVal, true);
            // Local to var of phi-node
            auto *local = wrapperCall->add_args()->mutable_local();
            local->set_allocated_origin(
                llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
            local->mutable_ref()->set_id(colVar->id());
        } else if (auto *arg = llvm::dyn_cast<llvm::Argument>(llvmVal)) {
            col::Variable *colVar = &colFResult.getFuncArgMapEntry(*arg);
            auto *argExpr = wrapperCall->add_args()->mutable_local();
            argExpr->set_allocated_origin(
                llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
            argExpr->mutable_ref()->set_id(colVar->id());
        } else {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Unable to map DIVariable to col-variable (Unsupported value).",
                *llvmParentFunc);
            return false;
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