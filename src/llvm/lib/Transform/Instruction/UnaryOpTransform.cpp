#include "Transform/Instruction/UnaryOpTransform.h"
#include "Transform/BlockTransform.h"

const std::string SOURCE_LOC = "Transform::Instruction::UnaryOp";
void llvm2col::transformUnaryOp(llvm::Instruction &llvmInstruction,
                                col::Block &colBlock,
                                pallas::FunctionCursor &funcCursor) {
    // TODO stub
    reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
}
