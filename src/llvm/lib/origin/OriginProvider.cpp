#include "Origin/OriginProvider.h"

#include <llvm/IR/Module.h>

#include "nlohmann/json.hpp"

#include "Origin/PreferredNameDeriver.h"
#include "Origin/ContextDeriver.h"
#include "Origin/ShortPositionDeriver.h"

namespace llvm2Col {
    using json = nlohmann::json;

    const std::string PREFERRED_NAME = "preferredName";
    const std::string CONTEXT = "context";
    const std::string INLINE_CONTEXT = "inlineContext";
    const std::string SHORT_POSITION = "shortPosition";

    std::string generateProgramOrigin(llvm::Module &llvmModule) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, "program:" + llvmModule.getName().str()});
        originMap.insert({CONTEXT, deriveModuleContext(llvmModule)});
        originMap.insert({INLINE_CONTEXT, deriveModuleContext(llvmModule)});
        originMap.insert({SHORT_POSITION, deriveModuleShortPosition(llvmModule)});

        return json(originMap).dump();
    }

    std::string generateFuncDefOrigin(llvm::Function &llvmFunction) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, llvmFunction.getName().str()});
        originMap.insert({CONTEXT, deriveFunctionContext(llvmFunction)});
        originMap.insert({INLINE_CONTEXT, deriveFunctionContext(llvmFunction)});
        originMap.insert({SHORT_POSITION, deriveFunctionShortPosition(llvmFunction)});

        return json(originMap).dump();
    }

    std::string generateFunctionContractOrigin(llvm::Function &llvmFunction, const std::string& contract) {
        std::unordered_map<std::string, std::string> originMap;

        // no prefered name
        originMap.insert({CONTEXT, contract});
        originMap.insert({INLINE_CONTEXT, contract});
        originMap.insert({SHORT_POSITION, deriveFunctionShortPosition(llvmFunction)});


        return json(originMap).dump();
    }

    std::string generateGlobalValOrigin(llvm::Module &llvmModule, const std::string &globVal) {
        std::unordered_map<std::string, std::string> originMap;

        // no prefered name
        originMap.insert({CONTEXT, globVal});
        originMap.insert({INLINE_CONTEXT, globVal});
        originMap.insert({SHORT_POSITION, deriveModuleShortPosition(llvmModule)});

        return json(originMap).dump();

    }

    std::string generateArgumentOrigin(llvm::Argument &llvmArgument) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, deriveArgumentPreferredName(llvmArgument)});
        originMap.insert({CONTEXT, deriveFunctionContext(*llvmArgument.getParent())});
        originMap.insert({INLINE_CONTEXT, deriveFunctionContext(*llvmArgument.getParent())});
        originMap.insert({SHORT_POSITION, deriveFunctionShortPosition(*llvmArgument.getParent())});

        return json(originMap).dump();
    }

    std::string generateBlockOrigin(llvm::BasicBlock &llvmBlock) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, "block"});
        originMap.insert({CONTEXT, deriveBlockContext(llvmBlock)});
        originMap.insert({INLINE_CONTEXT, deriveBlockContext(llvmBlock)});
        originMap.insert({SHORT_POSITION, deriveBlockShortPosition(llvmBlock)});

        return json(originMap).dump();
    }

    std::string generateLabelOrigin(llvm::BasicBlock &llvmBlock) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, "label"});
        originMap.insert({CONTEXT, deriveLabelContext(llvmBlock)});
        originMap.insert({INLINE_CONTEXT, deriveLabelContext(llvmBlock)});
        originMap.insert({SHORT_POSITION, deriveBlockShortPosition(llvmBlock)});

        return json(originMap).dump();
    }

    std::string generateSingleStatementOrigin(llvm::Instruction &llvmInstruction) {
        std::unordered_map<std::string, std::string> originMap;

        // omit preferred name
        originMap.insert({CONTEXT, deriveSurroundingInstructionContext(llvmInstruction)});
        originMap.insert({INLINE_CONTEXT, deriveInstructionContext(llvmInstruction)});
        originMap.insert({SHORT_POSITION, deriveInstructionShortPosition(llvmInstruction)});

        return json(originMap).dump();
    }

    std::string generateAssignTargetOrigin(llvm::Instruction &llvmInstruction) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, "var"});
        originMap.insert({CONTEXT, deriveInstructionContext(llvmInstruction)});
        originMap.insert({INLINE_CONTEXT, deriveInstructionLhs(llvmInstruction)});
        originMap.insert({SHORT_POSITION, deriveInstructionShortPosition(llvmInstruction)});

        return json(originMap).dump();
    }

    std::string generateBinExprOrigin(llvm::Instruction &llvmInstruction) {
        std::unordered_map<std::string, std::string> originMap;

        // omit preferred name
        originMap.insert({CONTEXT, deriveSurroundingInstructionContext(llvmInstruction)});
        originMap.insert({INLINE_CONTEXT, deriveInstructionRhs(llvmInstruction)});
        originMap.insert({SHORT_POSITION, deriveInstructionShortPosition(llvmInstruction)});

        return json(originMap).dump();
    }

    std::string generateFunctionCallOrigin(llvm::CallInst &callInstruction) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, callInstruction.getCalledFunction()->getName().str()});
        originMap.insert({CONTEXT, deriveSurroundingInstructionContext(callInstruction)});
        originMap.insert({INLINE_CONTEXT, deriveInstructionRhs(callInstruction)});
        originMap.insert({SHORT_POSITION, deriveInstructionShortPosition(callInstruction)});

        return json(originMap).dump();

    }

    std::string generateOperandOrigin(llvm::Instruction &llvmInstruction, llvm::Value &llvmOperand) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, deriveOperandPreferredName(llvmOperand)});
        originMap.insert({CONTEXT, deriveInstructionContext(llvmInstruction)});
        originMap.insert({INLINE_CONTEXT, deriveOperandContext(llvmOperand)});
        originMap.insert({SHORT_POSITION, deriveInstructionShortPosition(llvmInstruction)});

        return json(originMap).dump();
    }

    std::string generateTypeOrigin(llvm::Type &llvmType) {
        std::unordered_map<std::string, std::string> originMap;

        originMap.insert({PREFERRED_NAME, deriveTypePreferredName(llvmType)});

        return json(originMap).dump();
    }


}
