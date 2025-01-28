#ifndef PALLAS_FUNCLETPADOPTRANSFORM_H
#define PALLAS_FUNCLETPADOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformFuncletPadOp(llvm::Instruction &llvmInstruction,
                           col::Block &colBlock,
                           pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_FUNCLETPADOPTRANSFORM_H
