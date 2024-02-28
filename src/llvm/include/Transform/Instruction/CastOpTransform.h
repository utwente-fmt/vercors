#ifndef PALLAS_CASTOPTRANSFORM_H
#define PALLAS_CASTOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void convertCastOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                   pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_CASTOPTRANSFORM_H
