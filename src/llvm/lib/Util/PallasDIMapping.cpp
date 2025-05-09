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

llvm::DbgDeclareInst *
getUniqueDbgDeclare(llvm::ArrayRef<llvm::DbgVariableIntrinsic *> intrinsics) {
    llvm::DbgDeclareInst *dbgIntr = nullptr;
    for (auto *intr : intrinsics) {
        if (auto *dbgDecl = llvm::dyn_cast<llvm::DbgDeclareInst>(intr)) {
            if (dbgIntr == nullptr) {
                dbgIntr = dbgDecl;
            } else {
                // More than one dbg.declare
                return nullptr;
            }
        }
    }
    return dbgIntr;
}

} // namespace pallas::utils
