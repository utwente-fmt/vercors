#include "Transform/Instruction/UnaryOpTransform.h"
#include "Transform/BlockTransform.h"

namespace llvm2Col {
    const std::string SOURCE_LOC = "Transform::Instruction::UnaryOp";
    void transformUnaryOp(llvm::Instruction &llvmInstruction,
                          col::Block &colBlock,
                          vcllvm::FunctionCursor &funcCursor) {
        //TODO stub
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}