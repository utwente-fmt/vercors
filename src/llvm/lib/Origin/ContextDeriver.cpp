#include "Origin/ContextDeriver.h"

#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

// module derivers
std::string llvm2col::deriveModuleContext(llvm::Module &llvmModule) {
    std::string context;
    llvm::raw_string_ostream(context) << llvmModule;
    return context;
}

// function derivers
std::string llvm2col::deriveFunctionContext(llvm::Function &llvmFunction) {
    std::string context;
    llvm::raw_string_ostream(context) << llvmFunction;
    return context;
}

// block derivers
std::string llvm2col::deriveLabelContext(llvm::BasicBlock &llvmBlock) {
    if (llvmBlock.isEntryBlock()) {
        return "<entryBlock>";
    }
    std::string fullContext;
    llvm::raw_string_ostream(fullContext) << llvmBlock;
    return fullContext.substr(0, fullContext.find(':') + 1);
}

std::string llvm2col::deriveBlockContext(llvm::BasicBlock &llvmBlock) {
    std::string context;
    llvm::raw_string_ostream(context) << llvmBlock;
    return context;
}

// instruction derivers
std::string llvm2col::deriveSurroundingInstructionContext(
    llvm::Instruction &llvmInstruction) {
    std::string context;
    if (llvmInstruction.getPrevNode() != nullptr) {
        llvm::raw_string_ostream(context)
            << *llvmInstruction.getPrevNode() << '\n';
    }
    llvm::raw_string_ostream(context) << llvmInstruction;
    if (llvmInstruction.getNextNode() != nullptr) {
        llvm::raw_string_ostream(context) << '\n'
                                          << *llvmInstruction.getNextNode();
    }
    return context;
}

std::string
llvm2col::deriveInstructionContext(llvm::Instruction &llvmInstruction) {
    std::string context;
    llvm::raw_string_ostream(context) << llvmInstruction;
    return context;
}

std::string llvm2col::deriveGlobalVariableContext(
    llvm::GlobalVariable &llvmGlobalVariable) {
    std::string context;
    llvm::raw_string_ostream(context) << llvmGlobalVariable;
    return context;
}

std::string llvm2col::deriveInstructionLhs(llvm::Instruction &llvmInstruction) {
    std::string fullContext = deriveInstructionContext(llvmInstruction);
    return fullContext.substr(0, fullContext.find('='));
}

std::string llvm2col::deriveInstructionRhs(llvm::Instruction &llvmInstruction) {
    std::string fullContext = deriveInstructionContext(llvmInstruction);
    return fullContext.substr(fullContext.find('=') + 1);
}

std::string llvm2col::deriveOperandContext(llvm::Value &llvmOperand) {
    std::string context;
    llvm::raw_string_ostream contextStream = llvm::raw_string_ostream(context);
    llvmOperand.printAsOperand(contextStream, false);
    return context;
}
