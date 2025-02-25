#include "Util/PallasDIMapping.h"
#include "Util/Exceptions.h"

#include <string.h>

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Support/Casting.h>

const std::string SOURCE_LOC = "Util::PallasDIMapping";

namespace pallas::utils {

namespace {

llvm::SmallVector<llvm::DbgVariableIntrinsic *>
getIntrinsicsForDIVar(llvm::Function &f, llvm::DILocalVariable &diVar) {
    llvm::SmallVector<llvm::DbgVariableIntrinsic *> intrinsics;
    for (auto i = inst_begin(&f), end = inst_end(&f); i != end; ++i) {
        auto *asIntr = llvm::dyn_cast<llvm::DbgVariableIntrinsic>(&*i);
        // Check if the intrinsic actually uses the local variable.
        if (asIntr != nullptr && asIntr->getVariable() == &diVar)
            intrinsics.push_back(asIntr);
    }
    return intrinsics;
}

llvm::Value *mapDbgDeclare(const llvm::DbgDeclareInst &dbgDeclare) {
    if (dbgDeclare.getExpression() != nullptr &&
        dbgDeclare.getExpression()->getNumElements() != 0) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Unable to map DIVariable (DIExpressions not yet supported)");
        return nullptr;
    }
    return dbgDeclare.getAddress();
}

llvm::Value *
mapDbgValue(const llvm::SmallVector<llvm::DbgVariableIntrinsic *> &intrinsics,
            llvm::Loop *llvmLoop) {
    // Cast all intrinsics to dbg.value
    llvm::SmallVector<llvm::DbgValueInst *> dbgValues;
    for (auto *intr : intrinsics) {
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
        return dbgValues.front()->getValue();
    }

    // Try to find dbg.value that refers to phi-node in loop-header
    if (llvmLoop == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unable to map DIVariable (Ambiguous dbg.value)");
        return nullptr;
    }
    auto *loopHeader = llvmLoop->getHeader();
    llvm::Value *valInHeader = nullptr;
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
        valInHeader = phi;
    }

    return valInHeader;
}

} // namespace

llvm::Value *mapDIVarToValue(llvm::Function &f, llvm::DIVariable &diVar,
                             llvm::Loop *llvmLoop) {
    auto *locDiVar = llvm::dyn_cast<llvm::DILocalVariable>(&diVar);
    // TODO: Also support global variables
    if (locDiVar == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC,
            "Unable to map DIVariable (Global variables unsupported)");
        return nullptr;
    }

    // Check scope of DIVariable
    if ((!llvm::isa<llvm::DILocalScope>(locDiVar->getScope())) ||
        (llvm::cast<llvm::DILocalScope>(locDiVar->getScope())
             ->getSubprogram() != f.getSubprogram())) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unable to map DIVariable (Incorrect scope)");
        return nullptr;
    }

    // Get the debug-intrinsic that uses the local variable.
    llvm::SmallVector<llvm::DbgVariableIntrinsic *> intrinsics =
        getIntrinsicsForDIVar(f, *locDiVar);

    if (intrinsics.size() == 0) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Unable to map DIVariable (No intrinsic)");
        return nullptr;
    }

    // Try to map to a unique dbg.declare
    llvm::DbgDeclareInst *dbgDeclare = nullptr;
    for (auto *intr : intrinsics) {
        auto *declare = llvm::dyn_cast<llvm::DbgDeclareInst>(intr);
        if (declare == nullptr) {
            continue;
        }
        if (dbgDeclare != nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Unable to map DIVariable (Ambiguous dbg.declare)");
            return nullptr;
        }
        dbgDeclare = declare;
    }
    if (dbgDeclare != nullptr) {
        return mapDbgDeclare(*dbgDeclare);
    }
    return mapDbgValue(intrinsics, llvmLoop);
}

} // namespace pallas::utils
