#ifndef PALLAS_DIMAPPING_H
#define PALLAS_DIMAPPING_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Value.h>

#include <optional>
/**
 * Utility-functions for mapping debug-info to llvm instructions.
 */
namespace pallas::utils {

/**
 * Takes a function and a DIVariable that describes an argument of
 * the original source-function and attempts to map the DIVariable
 * to the corresponding argument of the llvm-function.
 * If the mapping is not possible, a nullptr is returned.
 */
llvm::Argument *mapDIVarToArg(llvm::Function &f, llvm::DIVariable &diVar);

/**
 * Mapps the arguments of the given function to DIVariable that refer to them. 
 * If the mapping is ambiguous, a nullopt is returned. 
 */
std::optional<llvm::DenseMap<llvm::Argument*, llvm::DILocalVariable *>>
mapArgsToDIVars(llvm::Function &f);

/**
 * Returns all dbg-variable intrinsics in the given function that refer to
 * the diven DILocalVariable.
 */
llvm::SmallVector<llvm::DbgVariableIntrinsic *>
getIntrinsicsForDIVar(llvm::Function &f, const llvm::DILocalVariable &diVar);

/**
 * If the given ArrayRef contains exactly one DbgDeclareInst, return a pointer
 * to this DbgDeclare. Otherwise, a nullpointer is returned.
 */
llvm::DbgDeclareInst *
getUniqueDbgDeclare(llvm::ArrayRef<llvm::DbgVariableIntrinsic *> intrinsics);

llvm::DIType *stripIgnored(llvm::DIType *type);

llvm::DIType *getDITypeForValue(llvm::Value &value);

} // namespace pallas::utils

#endif // PALLAS_DIMAPPING_H
