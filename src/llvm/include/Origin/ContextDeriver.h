#ifndef PALLAS_CONTEXTDERIVER_H
#define PALLAS_CONTEXTDERIVER_H

#include <llvm/IR/Value.h>

/**
 * Generators for VerCors origin objects context fields for various LLVM Value
 * types.
 *
 * For more info on VerCors origins see:
 * https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2col {
// module derivers
std::string deriveModuleContext(llvm::Module &llvmModule);

// function derivers
std::string deriveFunctionContext(llvm::Function &llvmFunction);

// block derivers
std::string deriveLabelContext(llvm::BasicBlock &llvmBlock);

std::string deriveBlockContext(llvm::BasicBlock &llvmBlock);

// instruction derivers
std::string
deriveSurroundingInstructionContext(llvm::Instruction &llvmInstruction);

std::string deriveInstructionContext(llvm::Instruction &llvmInstruction);

std::string
deriveGlobalVariableContext(llvm::GlobalVariable &llvmGlobalVariable);

std::string deriveInstructionLhs(llvm::Instruction &llvmInstruction);

std::string deriveInstructionRhs(llvm::Instruction &llvmInstruction);

// operand derivers
std::string deriveOperandContext(llvm::Value &llvmOperand);
} // namespace llvm2col
#endif // PALLAS_CONTEXTDERIVER_H
