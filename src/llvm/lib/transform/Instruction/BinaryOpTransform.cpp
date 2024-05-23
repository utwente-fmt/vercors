#include "Transform/Instruction/BinaryOpTransform.h"


#include "Transform/Transform.h"
#include "Transform/BlockTransform.h"
#include "Origin/OriginProvider.h"
#include "Util/Exceptions.h"

namespace llvm2Col {
    const std::string SOURCE_LOC = "Transform::Instruction::BinaryOp";

    void transformBinaryOp(llvm::Instruction &llvmInstruction,
                           col::Block &colBlock,
                           vcllvm::FunctionCursor &funcCursor) {
        col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(llvmInstruction, colBlock);
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
                if(llvmInstruction.isExact()) {
                    vcllvm::ErrorReporter::addError(SOURCE_LOC, "Exact division not supported", llvmInstruction);
                }
                col::FloorDiv &expr = *assignment.mutable_value()->mutable_floor_div();
                expr.set_allocated_blame(new col::Blame());
                transformBinExpr(llvmInstruction, expr, funcCursor);
                break;
            }
            default:
                reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
        }
    }
}
