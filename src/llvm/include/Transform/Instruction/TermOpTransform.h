#ifndef PALLAS_TERMOPTRANSFORM_H
#define PALLAS_TERMOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformTermOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                     pallas::FunctionCursor &funcCursor);

void transformRet(llvm::ReturnInst &llvmRetInstruction, col::Block &colBlock,
                  pallas::FunctionCursor &funcCursor);

void transformConditionalBranch(llvm::BranchInst &llvmBrInstruction,
                                col::Block &colBlock,
                                pallas::FunctionCursor &funcCursor);

void transformUnConditionalBranch(llvm::BranchInst &llvmBrInstruction,
                                  col::Block &colBlock,
                                  pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_TERMOPTRANSFORM_H
