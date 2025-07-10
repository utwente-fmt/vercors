#include "Passes/Function/PallasFunctionContractDeclarerPass.h"

#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasMD.h"
#include "Util/PallasWrapperUtils.h"

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
    if (assumedVal == nullptr || (!assumedVal->getBitWidth() == 1)) {
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
    auto &fam = mam.getResult<FunctionAnalysisManagerModuleProxy>(m)
                        .getManager();
    for (auto &f : m.functions()) {
        runOnFunction(f, fam);
    }
    return PreservedAnalyses::all();
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
    if (utils::hasVcllvmContract(f) ||
        !(utils::hasPallasContract(f) || utils::hasExternalPallasContract(f)))
        return;

    // Setup a fresh Pallas-contract
    FDCResult cResult = fam.getResult<FunctionContractDeclarer>(f);
    auto colPallasContract = cResult.getAssociatedColFuncContract()
                                 .mutable_pallas_function_contract();
    colPallasContract->set_allocated_blame(new col::Blame());

    // external-flag
    bool isExternal = utils::hasExternalPallasContract(f);
    colPallasContract->set_external(isExternal);

    // Get COL function
    FDResult fResult = fam.getResult<FunctionDeclarer>(f);
    // col::LlvmFunctionDefinition &colFunction =
    //     fResult.getAssociatedColFuncDef();

    col::ApplicableContract *colContract = colPallasContract->mutable_content();
    colContract->set_allocated_blame(new col::Blame());

    // Check wellformedness of the contract-metadata
    auto *contractNode = utils::getPallasContract(f);
    if (contractNode->getNumOperands() < 3) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Ill-formed contract. Expected at least 3 operands", f);
        return;
    }

    auto *mdSrcLoc = dyn_cast<MDNode>(contractNode->getOperand(0).get());
    if (!pallas::utils::isWellformedPallasLocation(mdSrcLoc)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract. First operand should encode source-location.",
            f);
        return;
    }

    // Build origin based on the source-location
    colPallasContract->set_allocated_origin(
        llvm2col::generatePallasFunctionContractOrigin(f, *mdSrcLoc));
    colContract->set_allocated_origin(
        llvm2col::generatePallasFunctionContractOrigin(f, *mdSrcLoc));

    // Set assumed-flag
    auto assumedFlag = getAssumedFlag(*contractNode);
    if (!assumedFlag.has_value()) {
        addError(f, "Malformed function contract (assumed-flag)");
        return;
    }
    colPallasContract->set_assumed(assumedFlag.value());

    // Handle contract clauses
    unsigned int clauseIdx = 3;
    while (clauseIdx < contractNode->getNumOperands()) {
        auto addClauseSuccess = addClauseToContract(
            *colContract, contractNode->getOperand(clauseIdx).get(), fam, f,
            clauseIdx - 2, *mdSrcLoc, isExternal);
        if (!addClauseSuccess)
            return;
        ++clauseIdx;
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
    const MDNode &clause, Function &parentFunc, FunctionAnalysisManager &fam) {
    // Get DIVariables from the MD-Node of the clause
    SmallVector<DIVariable *, 8> diVars;
    unsigned int vIdx = 3;
    while (vIdx < clause.getNumOperands()) {
        if (auto *diVar = dyn_cast<DIVariable>(clause.getOperand(vIdx).get())) {
            diVars.push_back(diVar);
        } else {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Ill-formed contract clause. Expected DIVariable as operand.",
                parentFunc);
            return std::nullopt;
        }
        vIdx++;
    }

    FDResult colFResult = fam.getResult<FunctionDeclarer>(parentFunc);

    // Resolve the DIVariables to col-variables
    SmallVector<col::Variable *, 8> colArgs;
    for (auto *diVar : diVars) {

        // Global values are not yet supported
        auto *localVar = dyn_cast<DILocalVariable>(diVar);
        if (localVar == nullptr || !localVar->isParameter()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC,
                "Ill-formed contract clause. Only arguments are currently "
                "supported ",
                parentFunc);
            return std::nullopt;
        }

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
    col::ApplicableContract &contract, Metadata *clauseOperand,
    FunctionAnalysisManager &fam, Function &parentFunc, unsigned int clauseNum,
    const MDNode &contractSrcLoc, const bool isExternal) {

    // Try to extract MDNode
    auto *clause = dyn_cast_if_present<MDNode>(clauseOperand);
    if (clause == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. Expected MDNode as operand.",
            parentFunc);
        return false;
    }

    // Check number of operands
    if ((!isExternal && clause->getNumOperands() < 3) ||
        (isExternal && clause->getNumOperands() != 3)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. Incorrect number of operands",
            parentFunc);
        return false;
    }

    // Check clause type (i.e. requires or ensures)
    auto *clauseTypeMD = dyn_cast<MDString>(clause->getOperand(0).get());
    if (clauseTypeMD == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. First operand should be a string.",
            parentFunc);
        return false;
    }
    auto clauseTypeStr = clauseTypeMD->getString().str();

    // Check source location
    auto *clauseSrcLoc = dyn_cast<MDNode>(clause->getOperand(1).get());
    if (clauseSrcLoc == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. Second operand should contain "
            "source location.",
            parentFunc);
        return false;
    }

    // Get pointer to the LLVM wrapper-function
    auto *wrapperF = getWrapperFuncFromClause(*clause, parentFunc);
    if (wrapperF == nullptr)
        return false;

    // Get COL representation of wrapper function
    auto wrapperFResult = fam.getResult<FunctionDeclarer>(*wrapperF);
    col::LlvmFunctionDefinition &colWrapperF =
        wrapperFResult.getAssociatedColFuncDef();

    // Get arguments for the wrapper-call
    auto wrapperArgs = isExternal ? getExternalContractArgs(parentFunc, fam)
                                  : getContractArgs(*clause, parentFunc, fam);
    if (!wrapperArgs.has_value()) {
        return false;
    }

    // Build a call to the wrapper-function with the gathered arguments
    col::LlvmFunctionInvocation *wrapperCall =
        new col::LlvmFunctionInvocation();
    wrapperCall->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(*wrapperF, *clauseSrcLoc));
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
            *wrapperF, *clauseSrcLoc));
        auto *varRef = argExpr->mutable_ref();
        varRef->set_id(v->id());
    }

    // Construct an AccountedPredicate that wraps the call to the
    // wrapper-function
    col::UnitAccountedPredicate *newPred = new col::UnitAccountedPredicate();
    newPred->set_allocated_origin(llvm2col::generatePallasFContractClauseOrigin(
        parentFunc, *clauseSrcLoc, clauseNum));
    newPred->mutable_pred()->set_allocated_llvm_function_invocation(
        wrapperCall);

    if (clauseTypeStr == pallas::constants::PALLAS_REQUIRES) {
        // Add to requires clauses
        if (!contract.has_requires_()) {
            contract.mutable_requires_()
                ->set_allocated_unit_accounted_predicate(newPred);
        } else {
            col::AccountedPredicate *oldPred = contract.release_requires_();
            auto *reqPred = contract.mutable_requires_();
            extendPredicate(reqPred,
                            llvm2col::generatePallasFunctionContractOrigin(
                                parentFunc, contractSrcLoc),
                            oldPred, newPred);
        }
    } else if (clauseTypeStr == pallas::constants::PALLAS_ENSURES) {
        // Add to ensures clauses
        if (!contract.has_ensures()) {
            contract.mutable_ensures()->set_allocated_unit_accounted_predicate(
                newPred);
        } else {
            col::AccountedPredicate *oldPred = contract.release_ensures();
            auto *ensPred = contract.mutable_ensures();
            extendPredicate(ensPred,
                            llvm2col::generatePallasFunctionContractOrigin(
                                parentFunc, contractSrcLoc),
                            oldPred, newPred);
        }
    } else {
        // Raise error
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Ill-formed contract clause. Unknown clause type.",
            parentFunc);
        return false;
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
            auto *arg =
                dyn_cast_if_present<Argument>(storeInst->getValueOperand());
            if (arg == nullptr || arg->getParent() != &f) {
                return nullptr;
            }
            return arg;
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

Function *PallasFunctionContractDeclarerPass::getWrapperFuncFromClause(
    MDNode &clause, Function &ctxFunc) {
    auto *wrapperFuncMD = dyn_cast<ValueAsMetadata>(clause.getOperand(2).get());
    if (wrapperFuncMD == nullptr || wrapperFuncMD->getValue() == nullptr ||
        !isa<Function>(wrapperFuncMD->getValue())) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. Second operand should be a "
            "pointer to a function.",
            ctxFunc);
        return nullptr;
    }

    // Check that the function is marked as a pallas wrapper function.
    auto *wrapperF = cast<Function>(wrapperFuncMD->getValue());
    if (!utils::isPallasExprWrapper(*wrapperF)) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Ill-formed contract clause. Second operand does not point to "
            "wrapper-function.",
            ctxFunc);
        return nullptr;
    }

    return wrapperF;
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
