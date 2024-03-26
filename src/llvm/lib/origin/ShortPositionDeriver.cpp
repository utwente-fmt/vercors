#include "Origin/ShortPositionDeriver.h"
#include "Origin/ContextDeriver.h"

namespace llvm2Col {
    const std::string POSITION_POINTER = "\n\t -> ";

    std::string deriveModuleShortPosition(llvm::Module &llvmModule) {
        return "file " + llvmModule.getSourceFileName();
    }

    std::string deriveFunctionShortPosition(llvm::Function &llvmFunction) {
        std::string functionPosition = deriveModuleShortPosition(*llvmFunction.getParent());
        llvm::raw_string_ostream functionPosStream = llvm::raw_string_ostream(functionPosition);
        functionPosStream << POSITION_POINTER << "function ";
        llvmFunction.printAsOperand(functionPosStream, false);
        return functionPosition;
    }

    std::string deriveBlockShortPosition(llvm::BasicBlock &llvmBlock) {
        std::string blockPosition = deriveFunctionShortPosition(*llvmBlock.getParent());
        llvm::raw_string_ostream blockPosStream = llvm::raw_string_ostream(blockPosition);
        blockPosStream << POSITION_POINTER << "block ";
        llvmBlock.printAsOperand(blockPosStream, false);
        blockPosStream << (llvmBlock.isEntryBlock() ? " (entryblock)" : "");
        return blockPosition;
    }

    std::string deriveInstructionShortPosition(llvm::Instruction &llvmInstruction) {
        std::string instructionPosition = deriveBlockShortPosition(*llvmInstruction.getParent());
        llvm::raw_string_ostream instructionPosStream = llvm::raw_string_ostream(instructionPosition);
        int pos = 0;
        llvm::BasicBlock *bb = llvmInstruction.getParent();
        for (auto &I: *bb) {
            pos++;
            if (&I == &llvmInstruction) {
                break;
            }
        }
        instructionPosStream << POSITION_POINTER << "instruction #" << pos << " ("
                             << deriveInstructionContext(llvmInstruction) << ')';
        return instructionPosition;
    }
}