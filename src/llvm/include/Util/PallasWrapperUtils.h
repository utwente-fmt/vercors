#ifndef PALLAS_WRAPPER_UTILS_H
#define PALLAS_WRAPPER_UTILS_H

#include "vct/col/ast/col.pb.h"

#include "Passes/Function/FunctionBodyTransformer.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>

/**
 * Utils for working with the wrapper-functions of Pallas's specifications
 */
namespace pallas::utils {

/**
 * Checks if the given debug-intrinsic has a non-empty DIExpression.
 */
bool hasDiExpression(llvm::DbgVariableIntrinsic &intr);

/**
 * Given an alloca-Instruction, this function adds an argument to the given
 * call of a wrapper function that dereferences the alloca.
 */
void buildArgExprFromAlloca(col::LlvmFunctionInvocation &wrapperCall,
                            unsigned int argIdx, llvm::AllocaInst &llvmAlloca,
                            llvm::Function &llvmWFunc, llvm::MDNode &srcLoc,
                            pallas::FunctionCursor &functionCursor);

/**
 * Given an llvm-value that was referenced by a dbg.value instrinsic,
 * this function adds an argument to the given call of a wrapper function that
 * reads the given value.
 * Returns true on success and false otherwise.
 */
bool buildArgExprFromDbgValue(col::LlvmFunctionInvocation &wrapperCall,
                              unsigned int argIdx, llvm::DbgValueInst &dbgVal,
                              llvm::Function &llvmWFunc, llvm::MDNode &srcLoc,
                              pallas::FunctionCursor &functionCursor,
                              llvm::Function &llvmParentFunc);

/**
 * Given an llvm-instruction and a list of debug-intrinsics,
 * find the intrinsic that is the 'closest predecessor' of the given
 * instruction.
 * Uses the DominatorTreeAnalysis.
 */
llvm::DbgValueInst *getClosestDbgValue(
    const llvm::SmallVector<llvm::DbgVariableIntrinsic *> &intrinsics,
    llvm::Instruction &llvmInstr, llvm::FunctionAnalysisManager &fam);

} // namespace pallas::utils

#endif // PALLAS_WRAPPER_UTILS_H