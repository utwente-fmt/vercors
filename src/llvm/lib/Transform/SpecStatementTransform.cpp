#include "Transform/SpecStatementTransform.h"
#include "Origin/OriginProvider.h"
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
    // Deconstruct the MD-Node
    if (llvmSpecBlock.getNumOperands() < 2) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Malformed specification block. (Expected at least two operands)",
            llvmInstr);
        return;
    }
    // Src-Location
    auto *srcLoc =
        llvm::dyn_cast<llvm::MDNode>(llvmSpecBlock.getOperand(0).get());
    if (!pallas::utils::isWellformedPallasLocation(srcLoc)) {
        pallas::ErrorReporter::addError(SOURCE_LOC,
                                        "Malformed specification block. "
                                        "(Expected location as first operand)",
                                        llvmInstr);
        return;
    }

    // Statements
    for (auto idx = 1; idx < llvmSpecBlock.getNumOperands(); ++idx) {
        const llvm::MDOperand &op = llvmSpecBlock.getOperand(idx);
        if (auto *stmnt = llvm::dyn_cast_if_present<llvm::MDNode>(op.get())) {
            transformSpecStmnt(*stmnt, llvmInstr, colBlock, functionCursor);
        } else {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Malformed specification block.", llvmInstr);
            return;
        }
    }
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
                      llvm::MDNode &srcLoc,
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

void llvm2col::transformSpecStmnt(llvm::MDNode &specStmnt,
                                  llvm::Instruction &llvmInstr,
                                  col::LlvmBasicBlock &colBlock,
                                  pallas::FunctionCursor &functionCursor) {
    // Deconstruct the MD-Node
    if (specStmnt.getNumOperands() < 3) {
        printSpecStmntError(llvmInstr, "Expected at least three operands");
        return;
    }
    // Statement-type
    auto *typeStrMD = dyn_cast<llvm::MDString>(specStmnt.getOperand(0).get());
    if (typeStrMD == nullptr) {
        printSpecStmntError(llvmInstr, "First operand should be a MDString");
        return;
    }
    // Src-location
    auto *srcLoc = llvm::dyn_cast<llvm::MDNode>(specStmnt.getOperand(1).get());
    if (!pallas::utils::isWellformedPallasLocation(srcLoc)) {
        printSpecStmntError(llvmInstr,
                            "Expected src-location as second operand");
        return;
    }
    // Wrapper-function
    auto *llvmWFunc = pallas::utils::getWrapperFunc(specStmnt.getOperand(2));
    if (llvmWFunc == nullptr) {
        printSpecStmntError(llvmInstr,
                            "Expected wrapper-function as third operand");
        return;
    }
    auto &fam = functionCursor.getFunctionAnalysisManager();
    col::LlvmFunctionDefinition &colWFunc =
        fam.getResult<pallas::FunctionDeclarer>(*llvmWFunc)
            .getAssociatedColFuncDef();
    // DI-Variables
    llvm::SmallVector<llvm::DIVariable *> diVars;
    for (auto idx = 3; idx < specStmnt.getNumOperands(); ++idx) {
        auto *diVar = llvm::dyn_cast_if_present<llvm::DIVariable>(
            specStmnt.getOperand(idx).get());
        if (diVar == nullptr) {
            printSpecStmntError(llvmInstr, "Expected DIVariable as operand");
            return;
        }
        diVars.push_back(diVar);
    }

    // Build call to wrapper-function
    auto *wCall = new col::LlvmFunctionInvocation();
    wCall->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(*llvmWFunc, *srcLoc));
    wCall->set_allocated_blame(new col::Blame());
    wCall->mutable_ref()->set_id(colWFunc.id());

    // Add arguments to wrapper-call
    for (auto [argIdx, diVar] : llvm::enumerate(diVars)) {
        bool ok = buildArgForDIVar(*diVar, llvmInstr, *wCall, *llvmWFunc,
                                   argIdx, *srcLoc, functionCursor);
        if (!ok) {
            return;
        }
    }

    // Build Assume or Assert node
    col::Block &body = pallas::bodyAsBlock(colBlock);
    if (typeStrMD->getString().str() == pallas::constants::PALLAS_ASSERT) {
        // Build Assert
        col::VctAssert *assert = body.add_statements()->mutable_vct_assert();
        assert->set_allocated_blame(new col::Blame());
        // TODO: Fix the origin
        assert->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, *srcLoc, "assert"));
        assert->mutable_res()->set_allocated_llvm_function_invocation(wCall);
    } else if (typeStrMD->getString().str() ==
               pallas::constants::PALLAS_ASSUME) {
        // Build Assume
        col::Assume *assume = body.add_statements()->mutable_assume();
        assume->set_allocated_origin(llvm2col::generatePallasSpecStmntOrigin(
            llvmInstr, *srcLoc, "assume"));
        assume->mutable_assn()->set_allocated_llvm_function_invocation(wCall);
    } else {
        printSpecStmntError(llvmInstr, "Unknown statement-type");
    }
}
