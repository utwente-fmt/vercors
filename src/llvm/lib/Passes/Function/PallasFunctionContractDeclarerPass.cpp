#include "Passes/Function/PallasFunctionContractDeclarerPass.h"

#include "IRSpec/PallasSpecDecoding.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Passes/Module/StructTDeclarer.h"
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
using GhostArgT = PallasFunctionContractDeclarerPass::GhostArgType;

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

PallasFunctionContractDeclarerPass::GhostArgType
PallasFunctionContractDeclarerPass::getGhostArgType(
    const irspec::FunctionContract &contract, const llvm::MDNode &gArgMD,
    llvm::Function &f, bool isGivenArg) {

    auto gArgDef = irspec::getGhostArgDef(&gArgMD);
    if (!gArgDef.has_value())
        return GhostArgT();

    auto currentType = GhostArgT();
    // Check the signature of the contract's clauses to determine the type of
    // the ghost argument
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
            return GhostArgT();
        }
        auto *mappedArg = utils::mapDIVarToArg(clause.getWrapper(), *diVar);
        if (mappedArg == nullptr) {
            std::string err = "Failed to get type for ghost-arg " +
                              gArgDef->name + " based on wrapper function " +
                              clause.getWrapper().getName().str();
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return GhostArgT();
        }

        auto newType = GhostArgT(*mappedArg);

        // Compare type to previously determined type to check consistency
        if (currentType.isValid() && newType != currentType) {
            std::string err =
                "Found conflicting types for ghost-arg " + gArgDef->name;
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return GhostArgT();
        }
        currentType = newType;
    }

    if (!currentType.isValid()) {
        std::string err =
            "Failed to determine type for ghost-arg " + gArgDef->name;
        ErrorReporter::addError(SOURCE_LOC, err, f);
    }
    return currentType;
}

void PallasFunctionContractDeclarerPass::transformGhostArg(
    const irspec::GhostArgDef &gArgDef, col::LlvmFunctionArgument *colArg,
    PallasFunctionContractDeclarerPass::GhostArgType &type,
    llvm::Function &parentFunc, FunctionAnalysisManager &fam) {
    assert(type.isValid());

    auto &mamProxy =
        fam.getResult<llvm::ModuleAnalysisManagerFunctionProxy>(parentFunc);
    auto *sdRes =
        mamProxy.getCachedResult<StructTDeclarer>(*parentFunc.getParent());
    assert(sdRes != nullptr);
    colArg->set_allocated_origin(
        llvm2col::generatePallasSpecOrigin(gArgDef.loc, gArgDef.name));
    auto *colVar = colArg->mutable_v();
    colVar->set_allocated_origin(
        llvm2col::generatePallasSpecOrigin(gArgDef.loc, gArgDef.name));
    llvm2col::setColNodeId(colVar);

    try {
        // Type
        llvm2col::transformAndSetType(*type.type, *colVar->mutable_t(), *sdRes);

        // Byval attribute
        if (type.hasByVal()) {
            auto *bvAttr = colArg->add_attributes()->mutable_llvm_by_val_arg();
            bvAttr->set_allocated_origin(
                llvm2col::generatePallasSpecOrigin(gArgDef.loc, "byval"));
            llvm2col::transformAndSetType(*type.byValType, *bvAttr->mutable_t(),
                                          *sdRes);
        }

        // Sret attribute
        if (type.hasSret()) {
            auto *sretAttr = colArg->add_attributes()->mutable_llvm_sret_arg();
            sretAttr->set_allocated_origin(
                llvm2col::generatePallasSpecOrigin(gArgDef.loc, "sret"));
            llvm2col::transformAndSetType(*type.sretType,
                                          *sretAttr->mutable_t(), *sdRes);
        }

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
    if (utils::hasVcllvmContract(f) || irspec::getContractMD(f) == nullptr)
        return;

    bool isExternal = irspec::hasExternalPallasContract(f);

    // Decode the MD-encoding
    auto *contractNode = irspec::getContractMD(f);
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

    // Build origin based on the source-location
    colPallasContract->set_allocated_origin(
        llvm2col::generatePallasFunctionContractOrigin(f, irContract->loc));

    // external-flag
    colPallasContract->set_external(isExternal);
    // Set assumed-flag
    colPallasContract->set_assumed(irContract->assumed);

    // Given-args
    for (const auto g : irContract->givenArgs) {
        auto gDef = irspec::getGhostArgDef(g);
        if (!gDef.has_value())
            return;
        auto gType = getGhostArgType(*irContract, *g, f, true);
        auto *colArg = colPallasContract->add_llvm_given_args();
        transformGhostArg(*gDef, colArg, gType, f, fam);
        cResult.addGhostArgMapEntry(*g, colArg->v());
    }

    // Yields-args
    for (const auto y : irContract->yieldsArgs) {
        auto yDef = irspec::getGhostArgDef(y);
        if (!yDef.has_value())
            return;
        auto yType = getGhostArgType(*irContract, *y, f, false);
        auto *colArg = colPallasContract->add_llvm_yields_args();
        transformGhostArg(*yDef, colArg, yType, f, fam);
        cResult.addGhostArgMapEntry(*y, colArg->v());
    }

    // Contract clauses
    for (size_t idx = 0; idx < irContract->clauses.size(); ++idx) {
        bool addClauseSuccess = addClauseToContract(
            *colPallasContract, *irContract, idx, fam, f, isExternal);
        if (!addClauseSuccess)
            return;
    }

    // Ensure, that the required fields of the contract are set.
    // I.e. add trivial clauses if they are currently empty.
    addEmptyRequires(*colPallasContract, f);
    addEmptyEnsures(*colPallasContract, f);
}

bool PallasFunctionContractDeclarerPass::addClauseToContract(
    col::PallasFunctionContract &contract,
    const irspec::FunctionContract &irContract, unsigned int clauseIdx,
    FunctionAnalysisManager &fam, Function &parentFunc, const bool isExternal) {

    auto &clause = irContract.clauses[clauseIdx];

    // Build a call to the wrapper-function with the gathered arguments
    auto *wrapperInv = new col::LlvmWrapperInvocation();
    llvm2col::buildContractWrapperInv(clause, parentFunc, *wrapperInv, fam,
                                      isExternal);

    // Construct an AccountedPredicate that wraps the call to the
    // wrapper-function
    col::UnitAccountedPredicate *newPred = new col::UnitAccountedPredicate();
    newPred->set_allocated_origin(llvm2col::generatePallasFContractClauseOrigin(
        parentFunc, clause.getLoc(), clauseIdx + 1));
    newPred->mutable_pred()->set_allocated_llvm_wrapper_invocation(wrapperInv);

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
    col::PallasFunctionContract &contract, Function &f) {

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
    col::PallasFunctionContract &contract, Function &f) {

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
    contrCount += irspec::hasExternalPallasContract(f) ? 1 : 0;
    contrCount += irspec::hasPallasContract(f) ? 1 : 0;
    contrCount += utils::hasVcllvmContract(f) ? 1 : 0;

    if (contrCount > 1) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "The function has multiple contracts!", f);
        return true;
    }
    return false;
}

} // namespace pallas
