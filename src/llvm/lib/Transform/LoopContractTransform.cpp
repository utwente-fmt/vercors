#include "Transform/LoopContractTransform.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Transform/Transform.h"
#include "Transform/WrapperCallTransform.h"
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

void addError(llvm::Instruction &inst, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC, msg, inst);
}

void addError(llvm::Metadata &md, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC, msg, &md);
}
} // namespace

void llvm2col::transformLoopContract(llvm::Loop &llvmLoop,
                                     col::LoopContract &colContract,
                                     pallas::FunctionCursor &functionCursor) {
    auto *loopContrMD = pallas::irspec::getLoopContractMD(llvmLoop);
    if (loopContrMD == nullptr) {
        initializeEmptyLoopContract(colContract);
        return;
    }

    auto irContract = pallas::irspec::getLoopContract(loopContrMD);
    if (!irContract.has_value())
        return;

    col::LlvmLoopContract *colInvariant =
        colContract.mutable_llvm_loop_contract();
    colInvariant->set_allocated_origin(
        generatePallasLoopContractOrigin(llvmLoop, irContract->loc));
    colInvariant->mutable_blame();

    for (const auto &inv : irContract->clauses) {
        if (!addInvariantToContract(inv, llvmLoop, *colInvariant,
                                    irContract->loc, functionCursor)) {
            return;
        }
    }
    return;
}

namespace {
llvm::DbgValueInst *selectDbgValue(
    const llvm::SmallVector<llvm::DbgVariableIntrinsic *> &intrinsics,
    llvm::BasicBlock &loopHeader, llvm::FunctionAnalysisManager &fam) {
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
    llvm::DbgValueInst *valInHeader = nullptr;
    for (auto *dbgValue : dbgValues) {
        auto *phi = llvm::dyn_cast<llvm::PHINode>(dbgValue->getValue());
        // Only consider dbg-intrinsics in the loop-header that refer to a
        // phi-node in the loop-header.
        if (dbgValue->getParent() != &loopHeader || phi == nullptr ||
            phi->getParent() != &loopHeader) {
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
        pallas::utils::getClosestDbgValue(intrinsics, loopHeader.front(), fam);
    return closestDbgValue;
}

} // namespace

llvm::DbgVariableIntrinsic *
loopInvVarMapper(llvm::DILocalVariable &diVar, llvm::Value &matchedValue,
                 llvm::FunctionAnalysisManager &fam) {

    auto *header = llvm::dyn_cast<llvm::BasicBlock>(&matchedValue);
    if (header == nullptr) {
        addError(diVar,
                 "Failed to map DIVar to intrinsic, expected BasicBlock.");
        return nullptr;
    }

    auto *pFunc = header->getParent();
    auto intrinsics = pallas::utils::getIntrinsicsForDIVar(*pFunc, diVar);

    if (intrinsics.empty()) {
        addError(diVar, "Unable to map DIVariable to intrinsic.");
        return nullptr;
    }

    // Try to map to unique dbg.declare
    if (auto *dbgDecl = pallas::utils::getUniqueDbgDeclare(intrinsics))
        return dbgDecl;

    if (auto *dbgVal = selectDbgValue(intrinsics, *header, fam))
        return dbgVal;

    addError(diVar, "Unable to map DIVariable to intrinsic.");
    return nullptr;
}

bool llvm2col::addInvariantToContract(
    const pallas::irspec::LoopInvariantClause &inv, llvm::Loop &llvmLoop,
    col::LlvmLoopContract &colContract,
    const pallas::irspec::SrcLoc &contractLoc,
    pallas::FunctionCursor &functionCursor) {

    pallas::FunctionAnalysisManager &fam =
        functionCursor.getFunctionAnalysisManager();
    llvm::Function *llvmParentF = llvmLoop.getHeader()->getParent();

    col::LlvmFunctionDefinition &colWFunc =
        fam.getResult<pallas::FunctionDeclarer>(inv.getWrapper())
            .getAssociatedColFuncDef();

    // pallas::FDResult &colFResult =
    //     fam.getResult<pallas::FunctionDeclarer>(*llvmParentFunc);

    // Build Call to wrapper-function:
    auto *wInv = new col::LlvmWrapperInvocation();
    llvm2col::buildWrapperInv(inv, *llvmLoop.getHeader(), *llvmParentF, *wInv,
                               functionCursor, loopInvVarMapper);

    // Append wrapper-call to loop-contract
    if (colContract.has_invariant()) {
        auto *oldInv = colContract.release_invariant();
        auto *newInv = colContract.mutable_invariant()->mutable_star();
        newInv->set_allocated_origin(
            generatePallasLoopContractOrigin(llvmLoop, contractLoc));
        newInv->set_allocated_left(oldInv);
        newInv->mutable_right()->set_allocated_llvm_wrapper_invocation(wInv);
    } else {
        colContract.mutable_invariant()->set_allocated_llvm_wrapper_invocation(
            wInv);
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