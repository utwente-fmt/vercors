#include "Passes/Function/PallasFunctionContractDeclarerPass.h"

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

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <optional>

namespace pallas {
const std::string SOURCE_LOC =
    "Passes::Function::PallasFunctionContractDeclarerPass";

using namespace llvm;

/*
 * Pallas Function Contract Declarer Pass
 */
PreservedAnalyses
PallasFunctionContractDeclarerPass::run(Module &m, ModuleAnalysisManager &mam) {
    auto &fam =
        mam.getResult<FunctionAnalysisManagerModuleProxy>(m).getManager();
    for (auto &f : m.functions()) {
        runOnFunction(f, fam);
    }
    return PreservedAnalyses::all();
}

llvm::Type *PallasFunctionContractDeclarerPass::getGhostArgType(
    const irspec::FunctionContract &contract, const llvm::MDNode &gArgMD,
    llvm::Function &f, bool isGivenArg) {

    auto gArgDef = irspec::getGhostArgDef(&gArgMD);
    if (!gArgDef.has_value())
        return nullptr;

    llvm::Type *currentType = nullptr;
    // Check the signature of the contract's claues to determine the type of the
    // ghost argument
    for (auto &clause : contract.clauses) {
        // Skip requires-clauses for yields args:
        if (!isGivenArg &&
            clause.getType() == irspec::ContractClauseType::REQUIRES)
            continue;

        // Map debug variable to LLVM-value and get type
        auto *diVar = clause.getVarForGhostDef(gArgMD);
        if (diVar == nullptr) {
            ErrorReporter::addError(
                SOURCE_LOC,
                "Failed to find DIVariable for ghost-arg definition ", f);
            return nullptr;
        }
        auto *mappedArg = utils::mapDIVarToArg(clause.getWrapper(), *diVar);
        if (mappedArg == nullptr) {
            std::string err =
                "Failed to get type for ghost-arg " + gArgDef->name;
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return nullptr;
        }
        auto newType = mappedArg->getType();

        // Compare type to previously determined type to check consistency
        if (currentType != nullptr && newType != currentType) {
            std::string err =
                "Found conflicting types for ghost-arg " + gArgDef->name;
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return nullptr;
        }
        currentType = newType;
    }

    if (currentType == nullptr) {
        std::string err =
            "Failed to determine type for ghost-arg " + gArgDef->name;
        ErrorReporter::addError(SOURCE_LOC, err, f);
    }
    return currentType;
}

void PallasFunctionContractDeclarerPass::transformGhostArg(
    const irspec::GhostArgDef &gArgDef, col::Variable *colVar, llvm::Type &type,
    llvm::Function &parentFunc) {
    const auto &dataLayout = parentFunc.getParent()->getDataLayout();
    colVar->set_allocated_origin(
        llvm2col::generatePallasSpecOrigin(gArgDef.loc, gArgDef.name));
    llvm2col::setColNodeId(colVar);
    try {
        llvm2col::transformAndSetType(type, *colVar->mutable_t(), dataLayout);
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << e.what() << " in ghost argument " << gArgDef.name;
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(),
                                        parentFunc);
    }
}

void PallasFunctionContractDeclarerPass::runOnFunction(
    Function &f, FunctionAnalysisManager &fam) {
    // Check that f does not have a VCLLVM AND a Pallas contract
    if (hasConflictingContract(f))
        return;
    // Skip, if f has a non-empty vcllvm-contract, or no contract at all
    // If it does not have a contract, we need an empty VCLLVM contract instead
    // of an empty Pallas contract. Otherwise the mechanism for loading
    // contracts from a PVL-file does not get invoked.
    if (utils::hasVcllvmContract(f) || utils::getPallasContract(f) == nullptr)
        return;

    bool isExternal = utils::hasExternalPallasContract(f);

    // Decode the MD-encoding
    auto *contractNode = utils::getPallasContract(f);
    auto decodedContract = irspec::getContract(contractNode);
    if (!decodedContract.has_value())
        return;

    // Move ownership of the decoded contract into the FDCResult
    FDCResult &cResult = fam.getResult<FunctionContractDeclarer>(f);
    cResult.setIRContract(decodedContract.value());
    auto *irContract = cResult.getIRContract();

    // Setup a fresh Pallas-contract
    auto colPallasContract = cResult.getAssociatedColFuncContract()
                                 .mutable_pallas_function_contract();
    colPallasContract->set_allocated_blame(new col::Blame());

    // external-flag
    colPallasContract->set_external(isExternal);
    // Set assumed-flag
    colPallasContract->set_assumed(irContract->assumed);

    // Get COL function
    FDResult fResult = fam.getResult<FunctionDeclarer>(f);

    col::ApplicableContract *colContract = colPallasContract->mutable_content();
    colContract->set_allocated_blame(new col::Blame());

    // Build origin based on the source-location
    colPallasContract->set_allocated_origin(
        llvm2col::generatePallasFunctionContractOrigin(f, irContract->loc));
    colContract->set_allocated_origin(
        llvm2col::generatePallasFunctionContractOrigin(f, irContract->loc));

    // Add given-args
    for (const auto g : irContract->givenArgs) {
        auto gDef = irspec::getGhostArgDef(g);
        if (!gDef.has_value())
            return;
        llvm::Type *gType = getGhostArgType(*irContract, *g, f, true);
        auto *colVar = colContract->add_given_args();
        transformGhostArg(*gDef, colVar, *gType, f);
        cResult.addGhostArgMapEntry(*g, *colVar);
    }

    // Add yields-args
    for (const auto y : irContract->yieldsArgs) {
        auto yDef = irspec::getGhostArgDef(y);
        if (!yDef.has_value())
            return;
        llvm::Type *yType = getGhostArgType(*irContract, *y, f, false);
        auto *colVar = colContract->add_yields_args();
        transformGhostArg(*yDef, colVar, *yType, f);
        cResult.addGhostArgMapEntry(*y, *colVar);
    }

    // Handle contract clauses
    for (size_t idx = 0; idx < irContract->clauses.size(); ++idx) {
        bool addClauseSuccess = addClauseToContract(*colContract, *irContract,
                                                    idx, fam, f, isExternal);
        if (!addClauseSuccess)
            return;
    }

    // Ensure, that the required fields of the contract are set.
    // I.e. add trivial clauses if they are currently empty.
    addEmptyRequires(*colContract, f);
    addEmptyEnsures(*colContract, f);
    addEmptyContextEverywhere(*colContract, f);
    addEmptyKernelInvariant(*colContract, f);
}

bool PallasFunctionContractDeclarerPass::addClauseToContract(
    col::ApplicableContract &contract,
    const irspec::FunctionContract &irContract, unsigned int clauseIdx,
    FunctionAnalysisManager &fam, Function &parentFunc, const bool isExternal) {

    auto &clause = irContract.clauses[clauseIdx];

    // Build a call to the wrapper-function with the gathered arguments
    col::LlvmFunctionInvocation *wrapperCall =
        new col::LlvmFunctionInvocation();
    llvm2col::buildContractWrapperCall(clause, parentFunc, *wrapperCall, fam,
                                       isExternal);

    // Construct an AccountedPredicate that wraps the call to the
    // wrapper-function
    col::UnitAccountedPredicate *newPred = new col::UnitAccountedPredicate();
    newPred->set_allocated_origin(llvm2col::generatePallasFContractClauseOrigin(
        parentFunc, clause.getLoc(), clauseIdx + 1));
    newPred->mutable_pred()->set_allocated_llvm_function_invocation(
        wrapperCall);

    if (clause.getType() == pallas::irspec::ContractClauseType::REQUIRES) {
        // Add to requires clauses
        if (!contract.has_requires_()) {
            contract.mutable_requires_()
                ->set_allocated_unit_accounted_predicate(newPred);
        } else {
            col::AccountedPredicate *oldPred = contract.release_requires_();
            auto *reqPred = contract.mutable_requires_();
            extendPredicate(reqPred,
                            llvm2col::generatePallasFunctionContractOrigin(
                                parentFunc, irContract.loc),
                            oldPred, newPred);
        }
    } else if (clause.getType() ==
               pallas::irspec::ContractClauseType::ENSURES) {
        // Add to ensures clauses
        if (!contract.has_ensures()) {
            contract.mutable_ensures()->set_allocated_unit_accounted_predicate(
                newPred);
        } else {
            col::AccountedPredicate *oldPred = contract.release_ensures();
            auto *ensPred = contract.mutable_ensures();
            extendPredicate(ensPred,
                            llvm2col::generatePallasFunctionContractOrigin(
                                parentFunc, irContract.loc),
                            oldPred, newPred);
        }
    }

    return true;
}

void PallasFunctionContractDeclarerPass::addEmptyRequires(
    col::ApplicableContract &contract, Function &f) {

    // If the contract already has a requires-clause, do nothing
    if (contract.has_requires_())
        return;

    // Build predicate for the requires-clause
    auto *requiresPred =
        contract.mutable_requires_()->mutable_unit_accounted_predicate();
    requiresPred->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "requires true;"));
    auto *requiresExpr = requiresPred->mutable_pred()->mutable_boolean_value();
    requiresExpr->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "true"));
    requiresExpr->set_value(true);
}

void PallasFunctionContractDeclarerPass::addEmptyEnsures(
    col::ApplicableContract &contract, Function &f) {

    // If the contract already has a requires-clause, do nothing
    if (contract.has_ensures())
        return;

    // Build predicate for the ensures-clause
    auto *ensuresPred =
        contract.mutable_ensures()->mutable_unit_accounted_predicate();
    ensuresPred->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "ensures true;"));
    auto *ensuresExpr = ensuresPred->mutable_pred()->mutable_boolean_value();
    ensuresExpr->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "true"));
    ensuresExpr->set_value(true);
}

void PallasFunctionContractDeclarerPass::addEmptyContextEverywhere(
    col::ApplicableContract &contract, Function &f) {

    // If the contract already has a contextEverywhere-clause, do nothing
    if (contract.has_context_everywhere())
        return;

    // Build expression for contextEverywhere
    auto *contextExpr =
        contract.mutable_context_everywhere()->mutable_boolean_value();
    contextExpr->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "true"));
    contextExpr->set_value(true);
}

void PallasFunctionContractDeclarerPass::addEmptyKernelInvariant(
    col::ApplicableContract &contract, Function &f) {
    if (contract.has_kernel_invariant())
        return;
    // Build expression for kernelInvariant
    auto *kernelInvariant =
        contract.mutable_kernel_invariant()->mutable_boolean_value();
    kernelInvariant->set_allocated_origin(
        llvm2col::generateFunctionContractOrigin(f, "true"));
    kernelInvariant->set_value(true);
}
void PallasFunctionContractDeclarerPass::extendPredicate(
    col::AccountedPredicate *newPred, col::Origin *newPredOrigin,
    col::AccountedPredicate *left, col::UnitAccountedPredicate *right) {
    auto *newSplitPred = newPred->mutable_split_accounted_predicate();
    newSplitPred->set_allocated_origin(newPredOrigin);
    newSplitPred->set_allocated_left(left);
    newSplitPred->mutable_right()->set_allocated_unit_accounted_predicate(
        right);
}

bool PallasFunctionContractDeclarerPass::hasConflictingContract(Function &f) {
    int contrCount = 0;
    contrCount += utils::hasExternalPallasContract(f) ? 1 : 0;
    contrCount += utils::hasPallasContract(f) ? 1 : 0;
    contrCount += utils::hasVcllvmContract(f) ? 1 : 0;

    if (contrCount > 1) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "The function has multiple contracts!", f);
        return true;
    }
    return false;
}

} // namespace pallas
