#ifndef PALLAS_SPECSTATEMENTTRANSFORM_H
#define PALLAS_SPECSTATEMENTTRANSFORM_H

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
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>

/**
 * Implements the transformation of specification statements and other
 * specifications that are attached to an instruction.
 */
namespace llvm2col {
namespace col = vct::col::ast;

void transformSpecStmntBlock(llvm::MDNode &llvmSpecBlock,
                             llvm::Instruction &llvmInstr,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &functionCursor);

void transformSpecStmnt(const pallas::irspec::SpecStatement &stmnt,
                        llvm::Instruction &llvmInstr,
                        col::LlvmBasicBlock &colBlock,
                        pallas::FunctionCursor &functionCursor);

/**
 * Function for mapping DILocalVariables to the corresponding dbg-intrinsic.
 * Tries to map the variable to a unique dbg.declare and if this fail to the
 * closest dbg.value.
 */
llvm::DbgVariableIntrinsic *stmntVarMapper(llvm::DILocalVariable &diVar,
                                           llvm::Value &matchedValue,
                                           llvm::FunctionAnalysisManager &fam);

} // namespace llvm2col

#endif // PALLAS_SPECSTATEMENTTRANSFORM_H
