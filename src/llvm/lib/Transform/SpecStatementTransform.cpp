#include "Transform/SpecStatementTransform.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Transform/WrapperCallTransform.h"
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

void printError(llvm::Instruction &inst, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed specification: " + msg, inst);
}

void printError(llvm::Metadata &md, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed specification: " + msg, &md);
}

} // namespace

llvm::DbgVariableIntrinsic *llvm2col::stmntVarMapper(llvm::DILocalVariable &diVar,
                                           llvm::Value &matchedValue,
                                           llvm::FunctionAnalysisManager &fam) {

    auto *inst = llvm::dyn_cast<llvm::Instruction>(&matchedValue);
    if (inst == nullptr) {
        printError(diVar,
                   "Failed to map DIVar to intrinsic, expected instruction.");
        return nullptr;
    }

    auto *pFunc = inst->getFunction();
    auto intrinsics = pallas::utils::getIntrinsicsForDIVar(*pFunc, diVar);

    if (intrinsics.empty()) {
        printError(diVar, "Unable to map DIVariable to intrinsic.");
        return nullptr;
    }

    // Try to map to unique dbg.declare
    if (intrinsics.size() == 1 &&
        llvm::isa<llvm::DbgDeclareInst>(intrinsics.front())) {
        return intrinsics.front();
    }

    // Search the dbg.value-intrinsic that is closest to the matched instruction
    auto *dbgValueIntr =
        pallas::utils::getClosestDbgValue(intrinsics, *inst, fam);
    if (dbgValueIntr == nullptr)
        printError(*inst, "Unable to map DIVariable to intrinsic.");
    return dbgValueIntr;
}

void llvm2col::transformSpecStmnt(const pallas::irspec::SpecStatement &stmnt,
                                  llvm::Instruction &llvmInstr,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &functionCursor) {

    // Build call to wrapper-function
    auto *wInv = new col::LlvmWrapperInvocation();
    llvm2col::buildWrapperInv(stmnt, llvmInstr, *llvmInstr.getFunction(), *wInv,
                              functionCursor, stmntVarMapper);

    // COL-node for the statement
    col::Block &body = pallas::bodyAsBlock(colBlock);
    if (stmnt.getType() == pallas::irspec::SpecStatementType::ASSERT) {
        // Build Assert
        col::VctAssert *assert = body.add_statements()->mutable_vct_assert();
        assert->set_allocated_blame(new col::Blame());
        // TODO: Fix the origin
        assert->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "assert"));
        assert->mutable_res()->set_allocated_llvm_wrapper_invocation(wInv);
    } else if (stmnt.getType() == pallas::irspec::SpecStatementType::ASSUME) {
        // Build Assume
        col::Assume *assume = body.add_statements()->mutable_assume();
        assume->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "assume"));
        assume->mutable_assn()->set_allocated_llvm_wrapper_invocation(wInv);
    } else if (stmnt.getType() == pallas::irspec::SpecStatementType::FOLD) {
        // Build Fold
        col::Fold *fold = body.add_statements()->mutable_fold();
        fold->set_allocated_blame(new col::Blame());
        fold->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "fold"));
        col::AmbiguousFoldTarget *target =
            fold->mutable_res()->mutable_ambiguous_fold_target();
        target->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "fold"));
        target->mutable_target()->set_allocated_llvm_wrapper_invocation(wInv);
    } else if (stmnt.getType() == pallas::irspec::SpecStatementType::UNFOLD) {
        // Build Unfold
        col::Unfold *unfold = body.add_statements()->mutable_unfold();
        unfold->set_allocated_blame(new col::Blame());
        unfold->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "unfold"));
        col::AmbiguousFoldTarget *target =
            unfold->mutable_res()->mutable_ambiguous_fold_target();
        target->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, stmnt.getLoc(), "unfold"));
        target->mutable_target()->set_allocated_llvm_wrapper_invocation(wInv);
    } else if (stmnt.getType() ==
               pallas::irspec::SpecStatementType::GHOST_ASSIGN) {
        // Get ghost-var from contract of parent:
        auto *pFunc = llvmInstr.getFunction();
        auto &pContrRes = functionCursor.getFDCResult(*pFunc);
        if (pContrRes.getIRContract() == nullptr) {
            printError(llvmInstr, "Unable to get contract of parent function");
            return;
        }
        auto targetDef =
            pallas::irspec::getGhostArgDef(stmnt.getAssignTarget());
        if (!targetDef.has_value())
            return;
        auto *gVar = pContrRes.getGhostArgMapEntry(*stmnt.getAssignTarget());
        if (gVar == nullptr) {
            printError(llvmInstr,
                       "Unable to get ghost var from parent function");
            return;
        }
        // Build assignment
        col::Block &body = pallas::bodyAsBlock(colBlock);
        auto *assign = body.add_statements()->mutable_llvm_ghost_assign();
        assign->set_allocated_blame(new col::Blame());
        assign->set_allocated_origin(llvm2col::generatePallasSpecOrigin(
            stmnt.getLoc(), "Assignment to" + targetDef->name));
        // Build assign target
        auto *target = assign->mutable_target()->mutable_local();
        target->set_allocated_origin(llvm2col::generatePallasSpecOrigin(
            stmnt.getLoc(), "Ghost assign to " + targetDef->name));
        target->mutable_ref()->set_id(gVar->id());
        // Call
        assign->mutable_value()->set_allocated_llvm_wrapper_invocation(wInv);
    } else {
        printError(llvmInstr, "Unknown statement-type");
    }
}
