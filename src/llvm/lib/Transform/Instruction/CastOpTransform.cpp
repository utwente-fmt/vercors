#include "Transform/Instruction/CastOpTransform.h"

#include "Transform/BlockTransform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::CastOp";
void llvm2col::convertCastOp(llvm::Instruction &llvmInstruction,
                             col::Block &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // TODO stub
    reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
}
