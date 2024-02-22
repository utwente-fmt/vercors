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
        // XXX: There is an assumption here that signed and unsigned division
        // are equal
        if (llvmInstruction.isExact()) {
            // XXX (Alexander): I'm not sure why we wouldn't support exact
            // division because it seems to me that it is simply a promise used
            // by optimisations that the right operand divides the left exactly
            /* pallas::ErrorReporter::addError( */
            /*     SOURCE_LOC, "Exact division not supported", llvmInstruction);
             */
        }
        col::FloorDiv &expr = *assignment.mutable_value()->mutable_floor_div();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    // TODO: All of these are currently bitwise operators, verify that works
    // correctly when operating on booleans in VerCors
    case llvm::Instruction::And: {
        col::BitAnd &expr = *assignment.mutable_value()->mutable_bit_and();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Or: {
        col::BitOr &expr = *assignment.mutable_value()->mutable_bit_or();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Xor: {
        col::BitXor &expr = *assignment.mutable_value()->mutable_bit_xor();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Shl: {
        col::BitShl &expr = *assignment.mutable_value()->mutable_bit_shl();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::LShr: {
        col::BitUShr &expr = *assignment.mutable_value()->mutable_bit_u_shr();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::AShr: {
        col::BitShr &expr = *assignment.mutable_value()->mutable_bit_shr();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}
