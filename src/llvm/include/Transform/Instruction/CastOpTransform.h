#ifndef PALLAS_CASTOPTRANSFORM_H
#define PALLAS_CASTOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformCastOp(llvm::Instruction &llvmInstruction,
                     col::LlvmBasicBlock &colBlock,
                     pallas::FunctionCursor &funcCursor);

void transformSExt(llvm::SExtInst &sextInstruction,
                   col::LlvmBasicBlock &colBlock,
                   pallas::FunctionCursor &funcCursor);

void transformZExt(llvm::ZExtInst &sextInstruction,
                   col::LlvmBasicBlock &colBlock,
                   pallas::FunctionCursor &funcCursor);

void transformTrunc(llvm::TruncInst &truncInstruction,
                    col::LlvmBasicBlock &colBlock,
                    pallas::FunctionCursor &funcCursor);

void transformFPExt(llvm::FPExtInst &fpextInstruction,
                    col::LlvmBasicBlock &colBlock,
                    pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_CASTOPTRANSFORM_H
