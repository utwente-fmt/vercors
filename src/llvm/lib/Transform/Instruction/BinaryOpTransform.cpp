#include "Transform/Instruction/BinaryOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

#include <llvm/IR/Constants.h>
#include <llvm/Support/Casting.h>

const std::string SOURCE_LOC = "Transform::Instruction::BinaryOp";

void llvm2col::transformBinaryOp(llvm::Instruction &llvmInstruction,
                                 col::LlvmBasicBlock &colBlock,
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
            // XXX (Alexander): I just remembered, it's because this would
            // return poison if rounding was needed
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Exact division not supported", llvmInstruction);
        }
        col::TruncDiv &expr = *assignment.mutable_value()->mutable_trunc_div();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        expr.set_allocated_blame(new col::Blame());
        break;
    }
    // TODO: All of these are currently bitwise operators, verify that works
    // correctly when operating on booleans in VerCors
    case llvm::Instruction::And: {
        col::BitAnd &expr = *assignment.mutable_value()->mutable_bit_and();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        expr.set_allocated_blame(new col::Blame());
        break;
    }
    case llvm::Instruction::Or: {
        col::BitOr &expr = *assignment.mutable_value()->mutable_bit_or();
        transformBitwiseBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::Xor: {
        transformBitwiseXor(llvmInstruction, assignment, funcCursor);
        break;
    }
    case llvm::Instruction::Shl: {
        col::BitShl &expr = *assignment.mutable_value()->mutable_bit_shl();
        transformBitwiseBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::LShr: {
        col::BitUShr &expr = *assignment.mutable_value()->mutable_bit_u_shr();
        transformBitwiseBinExpr(llvmInstruction, expr, funcCursor);
        break;
    }
    case llvm::Instruction::AShr: {
        llvm::IntegerType *ty =
            llvm::cast<llvm::IntegerType>(llvmInstruction.getType());
        col::BitShr &expr = *assignment.mutable_value()->mutable_bit_shr();
        transformBinExpr(llvmInstruction, expr, funcCursor);
        expr.set_allocated_blame(new col::Blame());
        expr.set_bits(ty->getBitWidth());
        break;
    }
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}

void llvm2col::transformBitwiseXor(llvm::Instruction &llvmInstruction,
                                   col::Assign &assignment,
                                   pallas::FunctionCursor &funcCursor) {
    // We encode some common cases manually, to avoid having to use bitvectors.
    llvm::Type *ty = llvmInstruction.getType();
    if (llvmInstruction.getType()->isIntegerTy(1)) {
        // Binary operation on booleans
        auto *rightAsConst = llvm::dyn_cast_if_present<llvm::ConstantInt>(
            llvmInstruction.getOperand(1));
        if (rightAsConst != nullptr && rightAsConst->isOne()) {
            // XOR %1 true  --> not %1
            col::Not *colNot = assignment.mutable_value()->mutable_not_();
            colNot->set_allocated_origin(
                generateBinExprOrigin(llvmInstruction));
            llvm2col::transformAndSetExpr(funcCursor, llvmInstruction,
                                          *llvmInstruction.getOperand(0),
                                          *colNot->mutable_arg());
            return;
        }
        auto *leftAsConst = llvm::dyn_cast_if_present<llvm::ConstantInt>(
            llvmInstruction.getOperand(0));
        if (leftAsConst != nullptr && leftAsConst->isOne()) {
            // XOR true %2  --> not %2
            col::Not *colNot = assignment.mutable_value()->mutable_not_();
            colNot->set_allocated_origin(
                generateBinExprOrigin(llvmInstruction));
            llvm2col::transformAndSetExpr(funcCursor, llvmInstruction,
                                          *llvmInstruction.getOperand(1),
                                          *colNot->mutable_arg());
            return;
        }
    }

    col::BitXor &expr = *assignment.mutable_value()->mutable_bit_xor();
    transformBitwiseBinExpr(llvmInstruction, expr, funcCursor);
}