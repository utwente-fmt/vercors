#include "Transform/Instruction/CastOpTransform.h"

#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

const std::string SOURCE_LOC = "Transform::Instruction::CastOp";
void llvm2col::transformCastOp(llvm::Instruction &llvmInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    switch (llvm::Instruction::CastOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::SExt:
        transformSExt(llvm::cast<llvm::SExtInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::ZExt:
        transformZExt(llvm::cast<llvm::ZExtInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::FPExt:
        transformFPExt(llvm::cast<llvm::FPExtInst>(llvmInstruction), colBlock,
                       funcCursor);
        break;
    case llvm::Instruction::Trunc:
        transformTrunc(llvm::cast<llvm::TruncInst>(llvmInstruction), colBlock,
                       funcCursor);
        break;
    case llvm::Instruction::PtrToInt:
        transformPtrToInt(llvm::cast<llvm::PtrToIntInst>(llvmInstruction),
                          colBlock, funcCursor);
        break;
    case llvm::Instruction::IntToPtr:
        transformIntToPtr(llvm::cast<llvm::IntToPtrInst>(llvmInstruction),
                          colBlock, funcCursor);
        break;
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}

void llvm2col::transformSExt(llvm::SExtInst &sextInstruction,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = sextInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(sextInstruction, colBlock);
    col::Expr *sextExpr = assignment.mutable_value();
    col::LlvmSignExtend *sext = sextExpr->mutable_llvm_sign_extend();
    sext->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(sextInstruction));
    llvm2col::transformAndSetValueType(*sextInstruction.getOperand(0), nullptr,
                                       *sext->mutable_input_type(), dataLayout);
    llvm2col::transformAndSetValueType(
        sextInstruction, nullptr, *sext->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, sextInstruction,
                                  *sextInstruction.getOperand(0),
                                  *sext->mutable_value());
}

void llvm2col::transformZExt(llvm::ZExtInst &zextInstruction,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = zextInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(zextInstruction, colBlock);
    col::Expr *zextExpr = assignment.mutable_value();
    col::LlvmZeroExtend *zext = zextExpr->mutable_llvm_zero_extend();
    zext->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(zextInstruction));
    llvm2col::transformAndSetValueType(*zextInstruction.getOperand(0), nullptr,
                                       *zext->mutable_input_type(), dataLayout);
    llvm2col::transformAndSetValueType(
        zextInstruction, nullptr, *zext->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, zextInstruction,
                                  *zextInstruction.getOperand(0),
                                  *zext->mutable_value());
}

void llvm2col::transformTrunc(llvm::TruncInst &truncInstruction,
                              col::LlvmBasicBlock &colBlock,
                              pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = truncInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(truncInstruction, colBlock);
    col::Expr *truncExpr = assignment.mutable_value();
    col::LlvmTruncate *trunc = truncExpr->mutable_llvm_truncate();
    trunc->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(truncInstruction));
    llvm2col::transformAndSetValueType(*truncInstruction.getOperand(0), nullptr,
                                       *trunc->mutable_input_type(),
                                       dataLayout);
    llvm2col::transformAndSetValueType(
        truncInstruction, nullptr, *trunc->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, truncInstruction,
                                  *truncInstruction.getOperand(0),
                                  *trunc->mutable_value());
}

void llvm2col::transformFPExt(llvm::FPExtInst &fpextInstruction,
                              col::LlvmBasicBlock &colBlock,
                              pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = fpextInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(fpextInstruction, colBlock);
    col::Expr *fpextExpr = assignment.mutable_value();
    col::LlvmFloatExtend *fpext = fpextExpr->mutable_llvm_float_extend();
    fpext->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(fpextInstruction));
    llvm2col::transformAndSetValueType(*fpextInstruction.getOperand(0), nullptr,
                                       *fpext->mutable_input_type(),
                                       dataLayout);
    llvm2col::transformAndSetValueType(
        fpextInstruction, nullptr, *fpext->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, fpextInstruction,
                                  *fpextInstruction.getOperand(0),
                                  *fpext->mutable_value());
}

void llvm2col::transformPtrToInt(llvm::PtrToIntInst &ptoiInstruction,
                                 col::LlvmBasicBlock &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = ptoiInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(ptoiInstruction, colBlock);
    col::Expr *castExpr = assignment.mutable_value();
    col::LlvmIntegerPointerCast *cast =
        castExpr->mutable_llvm_integer_pointer_cast();
    cast->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(ptoiInstruction));
    llvm2col::transformAndSetValueType(*ptoiInstruction.getOperand(0), nullptr,
                                       *cast->mutable_input_type(), dataLayout);
    llvm2col::transformAndSetValueType(
        ptoiInstruction, nullptr, *cast->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, ptoiInstruction,
                                  *ptoiInstruction.getOperand(0),
                                  *cast->mutable_value());
}

void llvm2col::transformIntToPtr(llvm::IntToPtrInst &itopInstruction,
                                 col::LlvmBasicBlock &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    const auto &dataLayout = itopInstruction.getModule()->getDataLayout();
    col::Assign &assignment =
        funcCursor.createAssignmentAndDeclaration(itopInstruction, colBlock);
    col::Expr *castExpr = assignment.mutable_value();
    col::LlvmIntegerPointerCast *cast =
        castExpr->mutable_llvm_integer_pointer_cast();
    cast->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(itopInstruction));
    llvm2col::transformAndSetValueType(*itopInstruction.getOperand(0), nullptr,
                                       *cast->mutable_input_type(), dataLayout);
    llvm2col::transformAndSetValueType(
        itopInstruction, nullptr, *cast->mutable_output_type(), dataLayout);
    llvm2col::transformAndSetExpr(funcCursor, itopInstruction,
                                  *itopInstruction.getOperand(0),
                                  *cast->mutable_value());
}
