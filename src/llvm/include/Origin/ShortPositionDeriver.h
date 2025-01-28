#ifndef PALLAS_SHORTPOSITIONDERIVER_H
#define PALLAS_SHORTPOSITIONDERIVER_H

#include <llvm/IR/Module.h>

/**
 * Generators for VerCors origin objects shortPosition fields for various LLVM
 * Value types.
 *
 * It generates a path from the highest level abstraction to the lowest in order
 * of Module -> Function -> Block -> Instruction.
 *
 * Each abstraction level calls its parent generator to generate its path (e.g.
 * deriveBlockShortPosition calls deriveFunctionShortPosition and
 * deriveInstructionShortPosition calls deriveBlockShortPosition)
 *
 * For more info on VerCors origins see:
 * https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2col {
std::string deriveModuleShortPosition(llvm::Module &llvmModule);

std::string deriveFunctionShortPosition(llvm::Function &llvmFunction);

std::string deriveBlockShortPosition(llvm::BasicBlock &llvmBlock);

std::string deriveInstructionShortPosition(llvm::Instruction &llvmInstruction);
} // namespace llvm2col
#endif // PALLAS_SHORTPOSITIONDERIVER_H
