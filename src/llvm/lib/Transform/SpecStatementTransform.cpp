#include "Transform/SpecStatementTransform.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Util/BlockUtils.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasMD.h"
#include "Util/PallasWrapperUtils.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Casting.h>
#include <string>

const std::string SOURCE_LOC = "Transform::SpecStatementTransform";

void llvm2col::transformSpecStmntBlock(llvm::MDNode &llvmSpecBlock,
                                       llvm::Instruction &llvmInstr,
                                       col::LlvmBasicBlock &colBlock,
                                       pallas::FunctionCursor &functionCursor) {

    // Decode the MD-node
    auto irStmntBlock = pallas::irspec::getSpecStatementBlock(&llvmSpecBlock);
    if (!irStmntBlock.has_value())
        return;

    for (auto &stmnt : irStmntBlock->statements)
        transformSpecStmnt(stmnt, llvmInstr, colBlock, functionCursor);
}

namespace {
namespace col = vct::col::ast;

void printSpecStmntError(llvm::Instruction &inst, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed spec-statement: " + msg, inst);
}

bool buildArgForDIVar(llvm::DIVariable &diVar, llvm::Instruction &llvmInstr,
                      col::LlvmFunctionInvocation &wrapperCall,
                      llvm::Function &llvmWrapperFunc, unsigned int argIdx,
                      const pallas::irspec::SrcLoc &srcLoc,
                      pallas::FunctionCursor &functionCursor) {
    llvm::DILocalVariable *diLocVar =
        llvm::dyn_cast<llvm::DILocalVariable>(&diVar);
    if (diLocVar == nullptr) {
        printSpecStmntError(llvmInstr,
                            "Global DIVariables are currently unsupported");
        return false;
    }

    // Function in which the instruction is located
    llvm::Function *parentFunc = llvmInstr.getFunction();

    llvm::SmallVector<llvm::DbgVariableIntrinsic *> intrinsics =
        pallas::utils::getIntrinsicsForDIVar(*parentFunc, *diLocVar);

    if (intrinsics.empty()) {
        printSpecStmntError(llvmInstr,
                            "Unable to map DIVariable to intrinsic.");
        return false;
    } else if (intrinsics.size() == 1 &&
               llvm::isa<llvm::DbgDeclareInst>(intrinsics.front())) {
        // Try to map to unique dbg.declare
        auto dbgDeclare = llvm::cast<llvm::DbgDeclareInst>(intrinsics.front());
        if (pallas::utils::hasDiExpression(*dbgDeclare)) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "DIExpressions are currently not supported.",
                *dbgDeclare);
            return false;
        }
        auto alloca =
            llvm::dyn_cast<llvm::AllocaInst>(dbgDeclare->getAddress());
        if (alloca == nullptr) {
            printSpecStmntError(llvmInstr,
                                "Currently, only alloca is supported "
                                "as a target for dbg.declare.");
            return false;
        }
        pallas::utils::buildArgExprFromAlloca(wrapperCall, argIdx, *alloca,
                                              llvmWrapperFunc, srcLoc,
                                              functionCursor);
    } else {
        // Search the dbg.value-intrinsic that is closest to the instruction
        // to which the spec-block is attached.
        auto &fam = functionCursor.getFunctionAnalysisManager();
        auto *dbgValueIntr =
            pallas::utils::getClosestDbgValue(intrinsics, llvmInstr, fam);
        if (dbgValueIntr == nullptr) {
            printSpecStmntError(llvmInstr,
                                "Unable to map dbg.value to instruction.");
            return false;
        }
        bool ok = pallas::utils::buildArgExprFromDbgValue(
            wrapperCall, argIdx, *dbgValueIntr, llvmWrapperFunc, srcLoc,
            functionCursor, *parentFunc);
        if (!ok) {
            printSpecStmntError(llvmInstr,
                                "Unable to build argument of wrapper-function");
            return false;
        }
    }
    return true;
}

} // namespace

void llvm2col::transformSpecStmnt(const pallas::irspec::SpecStatement &stmnt,
                                  llvm::Instruction &llvmInstr,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &functionCursor) {

    auto &fam = functionCursor.getFunctionAnalysisManager();
    col::LlvmFunctionDefinition &colWFunc =
        fam.getResult<pallas::FunctionDeclarer>(*stmnt.wrapperFunction)
            .getAssociatedColFuncDef();

    // Build call to wrapper-function
    auto *wCall = new col::LlvmFunctionInvocation();
    wCall->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
        *stmnt.wrapperFunction, stmnt.loc));
    wCall->set_allocated_blame(new col::Blame());
    wCall->mutable_ref()->set_id(colWFunc.id());

    // Add arguments to wrapper-call
    for (auto [argIdx, diVar] : llvm::enumerate(stmnt.wrapperArgs)) {
        bool ok =
            buildArgForDIVar(*diVar, llvmInstr, *wCall, *stmnt.wrapperFunction,
                             argIdx, stmnt.loc, functionCursor);
        if (!ok)
            return;
    }

    // Get contract from parent function
    auto *llvmParentFunc = llvmInstr.getParent()->getParent();
    auto &parentContrRes =
        fam.getResult<pallas::FunctionContractDeclarer>(*llvmParentFunc);
    if (parentContrRes.getIRContract() == nullptr &&
        (stmnt.givenArgs.size() > 0 || stmnt.yieldsArgs.size() > 0)) {
        printSpecStmntError(
            llvmInstr,
            "Unable to get ghost args from contract of parent function");
        return;
    }

    // Add ghost args to wrapper-call
    // TODO: De-duplicate this with LoopContractTransform
    llvm::SmallVector<col::Variable *> ghostArgVars;
    for (auto &gArg : parentContrRes.getIRContract()->givenArgs)
        ghostArgVars.push_back(parentContrRes.getGhostArgMapEntry(gArg));
    for (auto &yArg : parentContrRes.getIRContract()->yieldsArgs)
        ghostArgVars.push_back(parentContrRes.getGhostArgMapEntry(yArg));

    for (auto *v : ghostArgVars) {
        auto *argExpr = wCall->add_args()->mutable_local();
        argExpr->set_allocated_origin(llvm2col::generatePallasWrapperCallOrigin(
            *stmnt.wrapperFunction, stmnt.loc));
        argExpr->mutable_ref()->set_id(v->id());
    }

    // COL-node for the statement
    col::Block &body = pallas::bodyAsBlock(colBlock);
    if (stmnt.type == pallas::irspec::SpecStatementType::ASSERT) {
        // Build Assert
        col::VctAssert *assert = body.add_statements()->mutable_vct_assert();
        assert->set_allocated_blame(new col::Blame());
        // TODO: Fix the origin
        assert->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "assert"));
        assert->mutable_res()->set_allocated_llvm_function_invocation(wCall);
    } else if (stmnt.type == pallas::irspec::SpecStatementType::ASSUME) {
        // Build Assume
        col::Assume *assume = body.add_statements()->mutable_assume();
        assume->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "assume"));
        assume->mutable_assn()->set_allocated_llvm_function_invocation(wCall);
    } else if (stmnt.type == pallas::irspec::SpecStatementType::FOLD) {
        // Build Fold
        col::Fold *fold = body.add_statements()->mutable_fold();
        fold->set_allocated_blame(new col::Blame());
        fold->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "fold"));
        col::AmbiguousFoldTarget *target =
            fold->mutable_res()->mutable_ambiguous_fold_target();
        target->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "fold"));
        target->mutable_target()->set_allocated_llvm_function_invocation(wCall);
    } else if (stmnt.type == pallas::irspec::SpecStatementType::UNFOLD) {
        // Build Unfold
        col::Unfold *unfold = body.add_statements()->mutable_unfold();
        unfold->set_allocated_blame(new col::Blame());
        unfold->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "unfold"));
        col::AmbiguousFoldTarget *target =
            unfold->mutable_res()->mutable_ambiguous_fold_target();
        target->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.loc, "unfold"));
        target->mutable_target()->set_allocated_llvm_function_invocation(wCall);
    } else {
        printSpecStmntError(llvmInstr, "Unknown statement-type");
    }
}
