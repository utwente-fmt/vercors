#include "Util/PallasDIMapping.h"
#include "Util/Exceptions.h"
#include "Util/PallasWrapperUtils.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Support/Casting.h>
#include <llvm/Transforms/Utils/Local.h>

#include <sstream>

const std::string SOURCE_LOC = "Util::PallasDIMapping";

namespace pallas::utils {

std::optional<llvm::DenseMap<llvm::Argument *, llvm::DILocalVariable *>>
mapArgsToDIVars(llvm::Function &f) {
    // Collect all DIVariables that refer to arguments
    llvm::SmallSet<llvm::DILocalVariable *, 8> diVars;
    for (auto i = inst_begin(f), end = inst_end(f); i != end; ++i) {
        auto *intr = llvm::dyn_cast<llvm::DbgVariableIntrinsic>(&*i);
        if (intr == nullptr)
            continue;
        if (pallas::utils::hasDiExpression(*intr)) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "DIExpressions are not yet supported.", f);
            return std::nullopt;
        }
        auto *var = intr->getVariable();
        if (var->getArg() != 0)
            diVars.insert(var);
    }

    // Map the DIVariable to arguments:
    llvm::DenseMap<llvm::Argument *, llvm::DILocalVariable *> mapping;
    for (auto *diVar : diVars) {
        auto *arg = mapDIVarToArg(f, *diVar);
        if (arg == nullptr) {
            std::stringstream s;
            s << "Failed to map DebugVariable for '" << diVar->getName().str()
              << "' to argument of function '" << f.getName().str() << "'";
            pallas::ErrorReporter::addError(SOURCE_LOC, s.str(), f);
            return std::nullopt;
        }
        // Check that mapping is not ambiguous
        if (mapping.contains(arg)) {
            std::stringstream s;
            s << "Multiple DebugVariables were mapped to argument '"
              << arg->getName().str() << "' of function '" << f.getName().str()
              << "'";
            pallas::ErrorReporter::addError(SOURCE_LOC, s.str(), f);
            return std::nullopt;
        }
        mapping.insert({arg, diVar});
    }

    return std::move(mapping);
}

llvm::Argument *mapDIVarToArg(llvm::Function &f, llvm::DIVariable &diVar) {
    auto *locDiVar = llvm::dyn_cast<llvm::DILocalVariable>(&diVar);
    if (locDiVar == nullptr || !locDiVar->isParameter()) {
        return nullptr;
    }

    // Get the debug-intrinsic that uses the local variable.
    llvm::SmallVector<llvm::DbgVariableIntrinsic *, 8> intrinsics;
    for (auto i = inst_begin(&f), end = inst_end(&f); i != end; ++i) {
        auto *asIntr = llvm::dyn_cast<llvm::DbgVariableIntrinsic>(&*i);
        if (asIntr != nullptr && pallas::utils::hasDiExpression(*asIntr)) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "DIExpressions are not yet supported.", f);
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
            // TODO: De-duplicate with PallasWrapperUtils::buildArgExprFromDbgValue
            // Booleans are often extended to larger bitwidths and the 
            // debug-intrinsc is attached to the extended value. In this case, 
            // we must 'skip' the zext-instruction.
            intr->dump();
            if (valIntr->getValue()->getType()->isIntegerTy() &&
                !valIntr->getValue()->getType()->isIntegerTy(1) &&
                llvm::isa<llvm::ZExtInst>(valIntr->getValue())) {
                // Attempt to skip zext
                auto *zext = llvm::cast<llvm::ZExtInst>(valIntr->getValue());
                if (zext->getSrcTy()->isIntegerTy(1) &&
                    llvm::isa<llvm::Argument>(zext->getOperand(0)))
                    return llvm::cast<llvm::Argument>(zext->getOperand(0));
            }
        }
    }

    return nullptr;
}

llvm::SmallVector<llvm::DbgVariableIntrinsic *>
getIntrinsicsForDIVar(llvm::Function &f, const llvm::DILocalVariable &diVar) {
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
