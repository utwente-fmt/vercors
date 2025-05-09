#ifndef PALLAS_DIMAPPING_H
#define PALLAS_DIMAPPING_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Value.h>

/**
 * Utility-functions for mapping debug-info to llvm instructions.
 */
namespace pallas::utils {

/**
 * Returns all dbg-variable intrinsics in the given function that refer to
 * the diven DILocalVariable.
 */
llvm::SmallVector<llvm::DbgVariableIntrinsic *>
getIntrinsicsForDIVar(llvm::Function &f, llvm::DILocalVariable &diVar);

/**
 * If the given ArrayRef contains exactly one DbgDeclareInst, return a pointer
 * to this DbgDeclare. Otherwise, a nullpointer is returned.
 */
llvm::DbgDeclareInst *
getUniqueDbgDeclare(llvm::ArrayRef<llvm::DbgVariableIntrinsic *> intrinsics);

} // namespace pallas::utils

#endif // PALLAS_DIMAPPING_H
