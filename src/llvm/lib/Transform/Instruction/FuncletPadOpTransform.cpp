#include "Transform/Instruction/FuncletPadOpTransform.h"

#include "Transform/BlockTransform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::FuncletPadOp";

void llvm2col::transformFuncletPadOp(llvm::Instruction &llvmInstruction,
                                     col::Block &colBlock,
                                     pallas::FunctionCursor &funcCursor) {
    // TODO stub
    reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
}
