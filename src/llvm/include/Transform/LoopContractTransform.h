#ifndef PALLAS_LOOPCONTRACTTRANSFORM_H
#define PALLAS_LOOPCONTRACTTRANSFORM_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__

#include "IRSpec/PallasIRSpec.h"
#include "Passes/Function/FunctionBodyTransformer.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/PassManager.h>

/**
 * Implements the transformation of loop-contracts.
 */
namespace llvm2col {
namespace col = vct::col::ast;

void transformLoopContract(llvm::Loop &llvmLoop, col::LoopContract &colContract,
                           pallas::FunctionCursor &functionCursor);

void initializeEmptyLoopContract(col::LoopContract &colContract);

bool addInvariantToContract(llvm::MDNode &invMD, llvm::Loop &llvmLoop,
                            col::LlvmLoopContract &colContract,
                            const pallas::irspec::SrcLoc &contractLoc,
                            pallas::FunctionCursor &functionCursor);

} // namespace llvm2col

#endif // PALLAS_LOOPCONTRACTTRANSFORM_H
