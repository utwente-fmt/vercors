#ifndef PALLAS_WRAPPERCALLTRANSFORM_H
#define PALLAS_WRAPPERCALLTRANSFORM_H

#include "IRSpec/PallasIRSpec.h"
#include "Passes/Function/FunctionBodyTransformer.h"
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__

#include <functional>
#include <llvm/IR/Instruction.h>

/**
 * Implements the construction of calls to wrapper-functions.
 */
namespace llvm2col {
namespace col = vct::col::ast;

/**
 * Type for functions that map a DILocalVariable to a DbgVariableIntrinsic.
 */
typedef std::function<llvm::DbgVariableIntrinsic *(
    llvm::DILocalVariable &diVar, llvm::Value &matchedValue,
    llvm::FunctionAnalysisManager &fam)>
    varToIntrMapping;

/**
 * Initializes a call to a wrapper function based on the given
 * WrappedSpecElement.
 */
void buildWrapperInv(const pallas::irspec::WrappedSpecElement &specElem,
                     llvm::Value &matchedValue, llvm::Function &pFunc,
                     col::LlvmWrapperInvocation &colWrapperInv,
                     pallas::FunctionCursor &functionCursor,
                     varToIntrMapping diVarMapper);

/**
 * Initializes a call to a wrapper function for use in s function contract based
 * on the given WrappedSpecElement.
 */
void buildContractWrapperInv(const pallas::irspec::ContractClause &clause,
                             llvm::Function &pFunc,
                             col::LlvmWrapperInvocation &colWrapperInv,
                             llvm::FunctionAnalysisManager &fam,
                             bool isExternal);

/**
 * Initializes a call to a wrapper function belongs to an external contract
 * which does not have an explicit mapping of wrapper-args to values.
 */
void buildExternalWrapperInv(const pallas::irspec::WrappedSpecElement &specElem,
                             llvm::Function &pFunc,
                             col::LlvmWrapperInvocation &colWrapperInv,
                             llvm::FunctionAnalysisManager &fam);

bool buildArgForDIVar(llvm::DIVariable &diVar, llvm::Value &matchedValue,
                      const pallas::irspec::WrappedSpecElement &specElem,
                      col::LlvmWrapperInvocation &wrapperInv,
                      unsigned int argIdx,
                      pallas::FunctionCursor &functionCursor,
                      varToIntrMapping diVarMapper);

} // namespace llvm2col

#endif // PALLAS_WRAPPERCALLTRANSFORM_H
