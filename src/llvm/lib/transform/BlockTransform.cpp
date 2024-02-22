#include "Transform/BlockTransform.h"

#include "Transform/Instruction/TermOpTransform.h"
#include "Transform/Instruction/BinaryOpTransform.h"
#include "Transform/Instruction/UnaryOpTransform.h"
#include "Transform/Instruction/MemoryOpTransform.h"
#include "Transform/Instruction/FuncletPadOpTransform.h"
#include "Transform/Instruction/OtherOpTransform.h"
#include "Util/Exceptions.h"

namespace llvm2Col {
    const std::string SOURCE_LOC = "Transform::BlockTransform";

    void transformLlvmBlock(llvm::BasicBlock &llvmBlock, vcllvm::FunctionCursor &functionCursor) {
        col::Block &colBlock = functionCursor.getOrSetLlvmBlock2LabeledColBlockEntry(llvmBlock).block;
        for (auto *B: llvm::predecessors(&llvmBlock)) {
            if (!functionCursor.isVisited(*B)) return;
        }
        if (functionCursor.getLoopInfo().isLoopHeader(&llvmBlock)) {
            transformLoop(llvmBlock, functionCursor);
            return;
        }
        for (auto &I: llvmBlock) {
            transformInstruction(functionCursor, I, colBlock);
        }
        functionCursor.complete(colBlock);
    }

    void transformInstruction(vcllvm::FunctionCursor &funcCursor,
                              llvm::Instruction &llvmInstruction,
                              col::Block &colBodyBlock) {
        u_int32_t opCode = llvmInstruction.getOpcode();
        if (llvm::Instruction::TermOpsBegin <= opCode && opCode < llvm::Instruction::TermOpsEnd) {
            llvm2Col::transformTermOp(llvmInstruction, colBodyBlock, funcCursor);
        } else if (llvm::Instruction::BinaryOpsBegin <= opCode && opCode < llvm::Instruction::BinaryOpsEnd) {
            llvm2Col::transformBinaryOp(llvmInstruction, colBodyBlock, funcCursor);
        } else if (llvm::Instruction::UnaryOpsBegin <= opCode && opCode < llvm::Instruction::UnaryOpsEnd) {
            llvm2Col::transformUnaryOp(llvmInstruction, colBodyBlock, funcCursor);
        } else if (llvm::Instruction::MemoryOpsBegin <= opCode && opCode < llvm::Instruction::MemoryOpsEnd) {
            llvm2Col::transformMemoryOp(llvmInstruction, colBodyBlock, funcCursor);
        } else if (llvm::Instruction::FuncletPadOpsBegin <= opCode && opCode < llvm::Instruction::FuncletPadOpsEnd) {
            llvm2Col::transformFuncletPadOp(llvmInstruction, colBodyBlock, funcCursor);
        } else if (llvm::Instruction::OtherOpsBegin <= opCode && opCode < llvm::Instruction::OtherOpsEnd) {
            llvm2Col::transformOtherOp(llvmInstruction, colBodyBlock, funcCursor);
        } else {
            reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
        }
    }

    void transformLoop(llvm::BasicBlock &llvmBlock, vcllvm::FunctionCursor &functionCursor) {
        vcllvm::ErrorReporter::addError(SOURCE_LOC, "Unsupported loop detected", llvmBlock);
    }

    void reportUnsupportedOperatorError(const std::string &source, llvm::Instruction &llvmInstruction) {
        std::stringstream errorStream;
        errorStream << "Unsupported operator \"" << llvmInstruction.getOpcodeName() << '"';
        vcllvm::ErrorReporter::addError(source, errorStream.str(), llvmInstruction);
    }
}
