#include "Transform/Instruction/BinaryOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::BinaryOp";

void llvm2col::transformBinaryOp(llvm::Instruction &llvmInstruction,
                                 col::Block &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(llvmInstruction, colBlock);
    switch (llvm::Instruction::BinaryOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::Add: {
        col::Plus &expr = *assignment.mutable_value()->mutable_plus();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Sub: {
        col::Minus &expr = *assignment.mutable_value()->mutable_minus();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Mul: {
        col::Mult &expr = *assignment.mutable_value()->mutable_mult();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::SDiv:
    case llvm::Instruction::UDiv: {
        if (llvmInstruction.isExact()) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Exact division not supported", llvmInstruction);
        }
        col::FloorDiv &expr = *assignment.mutable_value()->mutable_floor_div();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}
