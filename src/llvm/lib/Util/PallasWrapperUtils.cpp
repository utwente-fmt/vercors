
#include "Util/PallasWrapperUtils.h"
#include "Origin/OriginProvider.h"
#include "Transform/Transform.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Type.h>

namespace pallas::utils {
namespace col = vct::col::ast;

bool hasDiExpression(llvm::DbgVariableIntrinsic &intr) {
    return intr.getExpression() != nullptr &&
           intr.getExpression()->getNumElements() != 0;
}

void buildArgExprFromAlloca(col::LlvmFunctionInvocation &wrapperCall,
                            unsigned int argIdx, llvm::AllocaInst &llvmAlloca,
                            llvm::Function &llvmWFunc, llvm::MDNode &srcLoc,
                            pallas::FunctionCursor &functionCursor) {
    col::Variable &colVar =
        functionCursor.getVariableMapEntry(llvmAlloca, false);
    // Ptr deref
    auto *ptrDeref = wrapperCall.add_args()->mutable_deref_pointer();
    ptrDeref->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(llvmWFunc, srcLoc));
    ptrDeref->set_allocated_blame(new col::Blame());

    // Cast, if type is packed struct with single element
    auto *structTy =
        llvm::dyn_cast<llvm::StructType>(llvmAlloca.getAllocatedType());
    llvm::Type *expectedTy = llvmWFunc.getArg(argIdx)->getType();
    bool isTrivialStruct = structTy != nullptr && structTy->isPacked() &&
                           structTy->getNumElements() == 1;

    col::Local *local = nullptr;
    if (structTy != expectedTy && isTrivialStruct &&
        structTy->getElementType(0) == expectedTy) {
        auto *cast = ptrDeref->mutable_pointer()->mutable_cast();
        cast->set_allocated_origin(
            llvm2col::generatePallasWrapperCallOrigin(llvmWFunc, srcLoc));
        col::TypeValue *tVal = cast->mutable_type_value()->mutable_type_value();
        tVal->set_allocated_origin(
            llvm2col::generatePallasWrapperCallOrigin(llvmWFunc, srcLoc));
        llvm2col::transformAndSetPointerType(*expectedTy,
                                             *tVal->mutable_value());
        local = cast->mutable_value()->mutable_local();
    } else {
        local = ptrDeref->mutable_pointer()->mutable_local();
    }

    // Local to var of alloca
    local->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(llvmWFunc, srcLoc));
    local->mutable_ref()->set_id(colVar.id());
}

bool buildArgExprFromDbgValue(col::LlvmFunctionInvocation &wrapperCall,
                              unsigned int argIdx, llvm::DbgValueInst &dbgVal,
                              llvm::Function &llvmWFunc, llvm::MDNode &srcLoc,
                              pallas::FunctionCursor &functionCursor,
                              llvm::Function &llvmParentFunc) {
    if (hasDiExpression(dbgVal)) {
        return false;
    }
    auto *llvmValue = dbgVal.getValue();

    col::Variable *colVar = nullptr;
    if (auto *arg = llvm::dyn_cast<llvm::Argument>(llvmValue)) {
        auto &fam = functionCursor.getFunctionAnalysisManager();
        auto &colFResult =
            fam.getResult<pallas::FunctionDeclarer>(llvmParentFunc);
        colVar = &colFResult.getFuncArgMapEntry(*arg);
    } else {
        colVar = &functionCursor.getVariableMapEntry(*llvmValue, true);
    }
    auto *argExpr = wrapperCall.add_args()->mutable_local();
    argExpr->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(llvmWFunc, srcLoc));
    argExpr->mutable_ref()->set_id(colVar->id());
    return true;
}

llvm::DbgValueInst *getClosestDbgValue(
    const llvm::SmallVector<llvm::DbgVariableIntrinsic *> &intrinsics,
    llvm::Instruction &llvmInstr, llvm::FunctionAnalysisManager &fam) {
    /* If this approach is not strong enough, an alternative approach would be
     * to use the dominator-tree to find the dbg.value that dominates the
     * llvmInstr but non of the other intrinsics.
     */
    auto *pFunc = llvmInstr.getParent()->getParent();
    llvm::DominatorTree &domTree = fam.getResult<DominatorTreeAnalysis>(*pFunc);

    // Build set of the llvm.value intrinsics
    llvm::SmallSet<llvm::DbgValueInst *, 8> dbgValues;
    for (auto *intr : intrinsics) {
        if (auto *dbgValue = llvm::dyn_cast<llvm::DbgValueInst>(intr)) {
            if (domTree.dominates(dbgValue, &llvmInstr)) {
                dbgValues.insert(dbgValue);
            }
        }
    }

    llvm::DbgValueInst *closestDbgValue = nullptr;
    // Find the closest dbg.value intrinsic that preceeds the instruction.
    // i.e. find the intrinsic that does not dominate any of the other
    // intrinsics.
    for (auto *dbgVal : dbgValues) {
        bool dominatesAny = false;
        for (auto *v : dbgValues) {
            if (dbgVal == v)
                continue;
            if (domTree.dominates(dbgVal, v)) {
                dominatesAny = true;
                break;
            }
        }
        if (!dominatesAny && closestDbgValue != nullptr) {
            // Ambiguous
            return nullptr;
        } else if (!dominatesAny) {
            closestDbgValue = dbgVal;
        }
    }
    return closestDbgValue;
}
} // namespace pallas::utils