#ifndef PALLAS_UNARYOPTRANSFORM_H
#define PALLAS_UNARYOPTRANSFORM_H

#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformUnaryOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                      pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_UNARYOPTRANSFORM_H
