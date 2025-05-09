#ifndef PALLAS_SPECSTATEMENTTRANSFORM_H
#define PALLAS_SPECSTATEMENTTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"
#include "vct/col/ast/col.pb.h"
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>

/**
 * Implements the transformation of specification statements.
 */
namespace llvm2col {
namespace col = vct::col::ast;

void transformSpecStmntBlock(llvm::MDNode &llvmSpecBlock,
                             llvm::Instruction &llvmInstr,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &functionCursor);

void transformSpecStmnt(llvm::MDNode &specStmnt, llvm::Instruction &llvmInstr,
                        col::LlvmBasicBlock &colBlock,
                        pallas::FunctionCursor &functionCursor);

} // namespace llvm2col

#endif // PALLAS_SPECSTATEMENTTRANSFORM_H