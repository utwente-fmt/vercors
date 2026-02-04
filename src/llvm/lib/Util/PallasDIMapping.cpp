#include "Util/PallasDIMapping.h"
#include "Util/Exceptions.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Support/Casting.h>
#include <llvm/Transforms/Utils/Local.h>

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

llvm::DIType *stripIgnored(llvm::DIType *type) {
    switch (type->getTag()) {
    case llvm::dwarf::DW_TAG_typedef:
    case llvm::dwarf::DW_TAG_const_type:
    case llvm::dwarf::DW_TAG_volatile_type:
    case llvm::dwarf::DW_TAG_shared_type:
    case llvm::dwarf::DW_TAG_atomic_type:
    case llvm::dwarf::DW_TAG_immutable_type:
        return stripIgnored(
            llvm::cast<llvm::DIDerivedType>(type)->getBaseType());
    default:
        return type;
    }
}

llvm::DIType *getDITypeForValue(llvm::Value &value) {
    llvm::DIType *ret = nullptr;
    llvm::SmallVector<llvm::DbgVariableIntrinsic *, 1> dbgVars;
    llvm::findDbgUsers(dbgVars, &value);
    for (auto user : dbgVars) {
        if (auto *val = llvm::dyn_cast<llvm::DbgValueInst>(user)) {
            auto stripped = stripIgnored(val->getVariable()->getType());
            if (ret != nullptr && ret != stripped) {
                ErrorReporter::addError(
                    SOURCE_LOC,
                    "Found multiple DbgVariableIntrinsics for value");
                return ret;
            }
            ret = stripped;
        } else if (auto *decl = llvm::dyn_cast<llvm::DbgDeclareInst>(user)) {
            auto stripped = stripIgnored(decl->getVariable()->getType());
            if (ret != nullptr &&
                (!llvm::isa<llvm::DIDerivedType>(ret) ||
                 llvm::cast<llvm::DIDerivedType>(ret)->getBaseType() !=
                     stripped)) {
                ErrorReporter::addError(
                    SOURCE_LOC,
                    "Found multiple DbgVariableIntrinsics for value");
                return ret;
            }
            llvm::DIBuilder builder(*decl->getModule());
            ret = builder.createArtificialType(builder.createPointerType(
                stripped,
                decl->getModule()->getDataLayout().getPointerTypeSizeInBits(
                    value.getType())));
        } else {
            ErrorReporter::addWarning(SOURCE_LOC,
                                      "Unsupported DbgVariableIntrinsic type");
        }
    }
    return ret;
}

} // namespace pallas::utils
