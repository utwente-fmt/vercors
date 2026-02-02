#include "Transform/LoopContractTransform.h"
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
    auto irContract = pallas::irspec::getLoopContract(
        pallas::utils::getPallasLoopContract(llvmLoop));
    if (!irContract.has_value()) {
        initializeEmptyLoopContract(colContract);
        return;
    }

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

bool llvm2col::addInvariantToContract(
    const pallas::irspec::LoopInvariantClause &inv, llvm::Loop &llvmLoop,
    col::LlvmLoopContract &colContract,
    const pallas::irspec::SrcLoc &contractLoc,
    pallas::FunctionCursor &functionCursor) {

    pallas::FunctionAnalysisManager &fam =
        functionCursor.getFunctionAnalysisManager();
    llvm::Function *llvmParentFunc = llvmLoop.getHeader()->getParent();

    col::LlvmFunctionDefinition &colWFunc =
        fam.getResult<pallas::FunctionDeclarer>(*inv.wrapperFunction)
            .getAssociatedColFuncDef();

    pallas::FDResult &colFResult =
        fam.getResult<pallas::FunctionDeclarer>(*llvmParentFunc);
    // col::LlvmFunctionDefinition &colParentFunc =
    //    colFResult.getAssociatedColFuncDef();

    // Build call to wrapper-function
    auto *wrapperCall = new col::LlvmFunctionInvocation();
    wrapperCall->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
        *inv.wrapperFunction, inv.loc));
    wrapperCall->set_allocated_blame(new col::Blame());
    wrapperCall->mutable_ref()->set_id(colWFunc.id());

    // Add arguments to wrapper-call
    for (auto [argIdx, diVar] : llvm::enumerate(inv.wrapperArgs)) {
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
                                                  *inv.wrapperFunction, inv.loc,
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
                *wrapperCall, argIdx, *dbgVal, *inv.wrapperFunction, inv.loc,
                functionCursor, *llvmParentFunc);
            if (!ok) {
                addError(*llvmParentFunc,
                         "Unable to build argument of wrapper-function.");
                return false;
            }
        }
    }

    // Get contract from parent function
    auto &parentContrRes =
        fam.getResult<pallas::FunctionContractDeclarer>(*llvmParentFunc);
    if (parentContrRes.getIRContract() == nullptr &&
        (inv.givenArgs.size() > 0 || inv.yieldsArgs.size() > 0)) {
        addError(*llvmParentFunc,
                 "Unable to get ghost args from contract of parent function");
        return false;
    }

    // Get col-variables for ghost-args from contract of parent function
    llvm::SmallVector<col::Variable *> ghostArgVars;
    for (auto &gArg : parentContrRes.getIRContract()->givenArgs)
        ghostArgVars.push_back(parentContrRes.getGhostArgMapEntry(gArg));
    for (auto &yArg : parentContrRes.getIRContract()->yieldsArgs)
        ghostArgVars.push_back(parentContrRes.getGhostArgMapEntry(yArg));

    // Extend call with ghost-args
    for (auto *v : ghostArgVars) {
        auto *argExpr = wrapperCall->add_args()->mutable_local();
        argExpr->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
            *inv.wrapperFunction, inv.loc));
        argExpr->mutable_ref()->set_id(v->id());
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