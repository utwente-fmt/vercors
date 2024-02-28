#ifndef PALLAS_BINARYOPTRANSFORM_H
#define PALLAS_BINARYOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformBinaryOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                       pallas::FunctionCursor &funcCursor);

} // namespace llvm2col
#endif // PALLAS_BINARYOPTRANSFORM_H
