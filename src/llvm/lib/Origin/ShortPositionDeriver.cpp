#include "Origin/ShortPositionDeriver.h"
#include "Origin/ContextDeriver.h"

const std::string POSITION_POINTER = "\n\t -> ";

std::string llvm2col::deriveModuleShortPosition(llvm::Module &llvmModule) {
    return "file " + llvmModule.getSourceFileName();
}

std::string
llvm2col::deriveFunctionShortPosition(llvm::Function &llvmFunction) {
    std::string functionPosition =
        deriveModuleShortPosition(*llvmFunction.getParent());
    llvm::raw_string_ostream functionPosStream =
        llvm::raw_string_ostream(functionPosition);
    functionPosStream << POSITION_POINTER << "function ";
    llvmFunction.printAsOperand(functionPosStream, false);
    return functionPosition;
}

std::string llvm2col::deriveBlockShortPosition(llvm::BasicBlock &llvmBlock) {
    std::string blockPosition =
        deriveFunctionShortPosition(*llvmBlock.getParent());
    llvm::raw_string_ostream blockPosStream =
        llvm::raw_string_ostream(blockPosition);
    blockPosStream << POSITION_POINTER << "block ";
    llvmBlock.printAsOperand(blockPosStream, false);
    blockPosStream << (llvmBlock.isEntryBlock() ? " (entryblock)" : "");
    return blockPosition;
}

std::string
llvm2col::deriveInstructionShortPosition(llvm::Instruction &llvmInstruction) {
    std::string instructionPosition =
        deriveBlockShortPosition(*llvmInstruction.getParent());
    llvm::raw_string_ostream instructionPosStream =
        llvm::raw_string_ostream(instructionPosition);
    int pos = 0;
    llvm::BasicBlock *bb = llvmInstruction.getParent();
    for (auto &I : *bb) {
        pos++;
        if (&I == &llvmInstruction) {
            break;
        }
    }
    instructionPosStream << POSITION_POINTER << "instruction #" << pos << " ("
                         << deriveInstructionContext(llvmInstruction) << ')';
    return instructionPosition;
}
