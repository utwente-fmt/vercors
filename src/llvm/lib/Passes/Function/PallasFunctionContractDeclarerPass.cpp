#include "Passes/Function/PallasFunctionContractDeclarerPass.h"

#include "IRSpec/PallasSpecDecoding.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Transform/Transform.h"
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

namespace {
void addError(llvm::Function &func, const std::string &msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC, msg, func);
}

std::optional<bool> getAssumedFlag(const MDNode &contractMD) {
    if (contractMD.getNumOperands() < 3)
        return std::nullopt;
    auto *constMD =
        dyn_cast<ConstantAsMetadata>(contractMD.getOperand(2).get());
    auto *assumedVal = dyn_cast_if_present<ConstantInt>(constMD->getValue());
    if (assumedVal == nullptr || (assumedVal->getBitWidth() != 1)) {
        return std::nullopt;
    }
    return assumedVal->isOne();
}

} // namespace

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
    const irspec::FunctionContract &contract, size_t argIdx, llvm::Function &f,
    bool isGivenArg) {

    llvm::Type *currentType = nullptr;
    for (auto &clause : contract.clauses) {
        // Map debug variable to LLVM-value and get type
        auto *diVar =
            isGivenArg ? clause.givenArgs[argIdx] : clause.yieldsArgs[argIdx];

        auto *mappedArg = mapDIVarToArg(*clause.wrapperFunction, *diVar);
        if (mappedArg == nullptr) {
            std::string err = "Failed to get type for ghost-arg at index " +
                              std::to_string(argIdx);
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return nullptr;
        }
        auto newType = mappedArg->getType();

        // Compare type to previously determined type to check consistency
        if (currentType != nullptr && newType != currentType) {
            std::string err =
                "Found conflicting types for ghost-arg at index " +
                std::to_string(argIdx);
            ErrorReporter::addError(SOURCE_LOC, err, f);
            return nullptr;
        }
        currentType = newType;
    }

    if (currentType == nullptr) {
        std::string err = "Failed to determine type for ghost-arg at index " +
                          std::to_string(argIdx);
        ErrorReporter::addError(SOURCE_LOC, err, f);
    }
    return currentType;
}

void PallasFunctionContractDeclarerPass::transformGhostArg(
    const irspec::GhostArgDef &gArgDef, col::Variable *colVar, llvm::Type &type,
    size_t idx, llvm::Function &parentFunc) {
    const auto &dataLayout = parentFunc.getParent()->getDataLayout();
    colVar->set_allocated_origin(
        llvm2col::generatePallasSpecOrigin(gArgDef.loc, gArgDef.name));
    llvm2col::setColNodeId(colVar);
    try {
        llvm2col::transformAndSetType(type, *colVar->mutable_t(), dataLayout);
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << e.what() << " in ghost argument #" << idx;
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

    bool isGhost = utils::hasPallasGhostContract(f);
    bool isExternal = utils::hasExternalPallasContract(f);
    bool implicitArgs = isGhost || isExternal;

    // Decode the MD-encoding
    auto *contractNode = utils::getPallasContract(f);
    auto irContract = irspec::getContract(contractNode, implicitArgs);
    if (!irContract.has_value())
        return;

    // Setup a fresh Pallas-contract
    FDCResult &cResult = fam.getResult<FunctionContractDeclarer>(f);
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
    for (const auto [idx, g] : llvm::enumerate(irContract->givenArgs)) {
        llvm::Type *gType = getGhostArgType(*irContract, idx, f, true);
        auto *colVar = colContract->add_given_args();
        transformGhostArg(g, colVar, *gType, idx, f);
        cResult.addGhostArgMapEntry(g, *colVar);
    }

    // Add yields-args
    for (const auto [idx, y] : llvm::enumerate(irContract->yieldsArgs)) {
        llvm::Type *gType = getGhostArgType(*irContract, idx, f, false);
        auto *colVar = colContract->add_yields_args();
        transformGhostArg(y, colVar, *gType, idx, f);
        cResult.addGhostArgMapEntry(y, *colVar);
    }

    // Handle contract clauses
    for (size_t idx = 0; idx < irContract->clauses.size(); ++idx) {
        bool addClauseSuccess = addClauseToContract(*colContract, *irContract,
                                                    idx, fam, f, implicitArgs);
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

std::optional<SmallVector<col::Variable *, 8>>
PallasFunctionContractDeclarerPass::getExternalContractArgs(
    Function &parentFunc, FunctionAnalysisManager &fam) {
    // For external function, return the arguments of the parent-function
    FDResult colFResult = fam.getResult<FunctionDeclarer>(parentFunc);

    SmallVector<col::Variable *, 8> colArgs;
    for (auto &arg : parentFunc.args()) {
        auto colArgVar = &colFResult.getFuncArgMapEntry(arg);
        colArgs.push_back(colArgVar);
    }

    return colArgs;
}

std::optional<SmallVector<col::Variable *, 8>>
PallasFunctionContractDeclarerPass::getContractArgs(
    const pallas::irspec::ContractClause &clause, Function &parentFunc,
    FunctionAnalysisManager &fam) {
    FDResult colFResult = fam.getResult<FunctionDeclarer>(parentFunc);

    // Resolve the DIVariables to col-variables
    SmallVector<col::Variable *, 8> colArgs;
    for (auto *localVar : clause.wrapperArgs) {
        // Check that the DIVariable belongs to the function to which the
        // contract is attached
        if (localVar->getScope() != parentFunc.getSubprogram()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Ill-formed contract clause. DIVariable does not belong to "
                "the function to which the contract is attached.",
                parentFunc);
            return std::nullopt;
        }

        auto llvmArg = mapDIVarToArg(parentFunc, *localVar);
        if (llvmArg == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable to argument.",
                parentFunc);
            return std::nullopt;
        }
        auto colArgVar = &colFResult.getFuncArgMapEntry(*llvmArg);
        colArgs.push_back(colArgVar);
    }
    return colArgs;
}

bool PallasFunctionContractDeclarerPass::addClauseToContract(
    col::ApplicableContract &contract, irspec::FunctionContract &irContract,
    unsigned int clauseIdx, FunctionAnalysisManager &fam, Function &parentFunc,
    const bool implicitArgs) {

    auto clause = irContract.clauses[clauseIdx];

    // Get COL representation of wrapper function
    auto wrapperFResult =
        fam.getResult<FunctionDeclarer>(*clause.wrapperFunction);
    auto &contrResult = fam.getResult<FunctionContractDeclarer>(parentFunc);
    col::LlvmFunctionDefinition &colWrapperF =
        wrapperFResult.getAssociatedColFuncDef();

    // Get arguments for the wrapper-call
    auto wrapperArgs = implicitArgs ? getExternalContractArgs(parentFunc, fam)
                                    : getContractArgs(clause, parentFunc, fam);
    if (!wrapperArgs.has_value()) {
        return false;
    }

    // Add ghost args
    for (auto &gArg : irContract.givenArgs) {
        auto *v = contrResult.getGhostArgMapEntry(gArg);
        if (v == nullptr)
            return false;
        wrapperArgs->push_back(v);
    }
    for (auto &yArg : irContract.yieldsArgs) {
        auto *v = contrResult.getGhostArgMapEntry(yArg);
        if (v == nullptr)
            return false;
        wrapperArgs->push_back(v);
    }

    // Build a call to the wrapper-function with the gathered arguments
    col::LlvmFunctionInvocation *wrapperCall =
        new col::LlvmFunctionInvocation();
    wrapperCall->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
        *clause.wrapperFunction, clause.loc));
    wrapperCall->set_allocated_blame(new col::Blame());

    // Build ref to parent function
    auto *fRef = wrapperCall->mutable_ref();
    fRef->set_id(colWrapperF.id());

    // Add argument-expression to invocation
    for (auto *v : *wrapperArgs) {
        // Construct Local-node that references the variable and add it to the
        // list of arguments
        auto *argExpr = wrapperCall->add_args()->mutable_local();
        // TODO: Currently this just points to the full clause.
        //       Could be extended to point to the specific variable instead.
        argExpr->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
            *clause.wrapperFunction, clause.loc));
        auto *varRef = argExpr->mutable_ref();
        varRef->set_id(v->id());
    }

    // Construct an AccountedPredicate that wraps the call to the
    // wrapper-function
    col::UnitAccountedPredicate *newPred = new col::UnitAccountedPredicate();
    newPred->set_allocated_origin(llvm2col::generatePallasFContractClauseOrigin(
        parentFunc, clause.loc, clauseIdx + 1));
    newPred->mutable_pred()->set_allocated_llvm_function_invocation(
        wrapperCall);

    if (clause.type == pallas::irspec::ContractClauseType::REQUIRES) {
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
    } else if (clause.type == pallas::irspec::ContractClauseType::ENSURES) {
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

Argument *PallasFunctionContractDeclarerPass::mapDIVarToArg(Function &f,
                                                            DIVariable &diVar) {
    auto *locDiVar = dyn_cast<DILocalVariable>(&diVar);
    if (locDiVar == nullptr || !locDiVar->isParameter()) {
        return nullptr;
    }

    // Get the debug-intrinsic that uses the local variable.
    SmallVector<DbgVariableIntrinsic *, 8> intrinsics;
    for (auto i = inst_begin(&f), end = inst_end(&f); i != end; ++i) {
        auto *asIntr = dyn_cast<DbgVariableIntrinsic>(&*i);
        if (asIntr != nullptr && pallas::utils::hasDiExpression(*asIntr)) {
            addError(f, "DIExpressions are not yet supported.");
            return nullptr;
        }
        if (asIntr != nullptr && asIntr->getVariable() == locDiVar)
            intrinsics.push_back(asIntr);
    }

    // Try to map to unique dbg.declare
    auto *declIntr = pallas::utils::getUniqueDbgDeclare(intrinsics);
    if (declIntr != nullptr) {
        if (auto *argument = dyn_cast<Argument>(declIntr->getAddress())) {
            return argument;
        }
        // Check if intrinsic refers to an alloca in the initial block of the
        // function that is set to the value of an argument in its first use.
        auto *alloc = dyn_cast_if_present<AllocaInst>(declIntr->getAddress());
        if (alloc == nullptr || !alloc->isUsedInBasicBlock(&f.getEntryBlock()))
            return nullptr;

        // Find all instructions that use the alloca
        SmallSet<Instruction *, 16> userInstr;
        for (User *user : alloc->users()) {
            if (auto *userInst = dyn_cast<Instruction>(user)) {
                userInstr.insert(userInst);
            }
        }

        // Check that the first user of the alloca is a store
        // that stores the value of an argument.
        for (auto &inst : f.getEntryBlock()) {
            if (!userInstr.contains(&inst)) {
                continue;
            }
            auto *storeInst = dyn_cast<StoreInst>(&inst);
            if (storeInst == nullptr) {
                return nullptr;
            }

            if (auto *arg = dyn_cast<Argument>(storeInst->getValueOperand())) {
                assert(arg->getParent() == &f);
                return arg;
            }
            if (auto *cast = dyn_cast<CastInst>(storeInst->getValueOperand())) {
                // We only go one layer deep here, but we might require more
                // depending on what compilers do
                if (auto *arg = dyn_cast<Argument>(cast->getOperand(0))) {
                    assert(arg->getParent() == &f);
                    return arg;
                }
            }
            return nullptr;
        }
        return nullptr;
    }
    // Try to map to dbg.value that refers directly to an argument of f
    for (auto *intr : intrinsics) {
        if (auto *valIntr = dyn_cast<DbgValueInst>(intr)) {
            auto *arg = dyn_cast_if_present<Argument>(valIntr->getValue());
            if (arg != nullptr && arg->getParent() == &f)
                return arg;
        }
    }

    return nullptr;
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
    contrCount += utils::hasPallasGhostContract(f) ? 1 : 0;
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
