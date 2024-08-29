#ifndef PALLAS_ORIGINPROVIDER_H
#define PALLAS_ORIGINPROVIDER_H

#include "vct/col/ast/Origin.pb.h"
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

/**
 * Generators for VerCors origin objects for various LLVM Value types.
 *
 * For more info on VerCors origins see:
 * https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2col {
namespace col = vct::col::ast;

col::Origin *generateProgramOrigin(llvm::Module &llvmModule);

col::Origin *generateFuncDefOrigin(llvm::Function &llvmFunction);

col::Origin *generateFunctionContractOrigin(llvm::Function &llvmFunction,
                                            const std::string &contract);

col::Origin *generateGlobalValOrigin(llvm::Module &llvmModule,
                                     const std::string &globVal);

col::Origin *generateArgumentOrigin(llvm::Argument &llvmArgument);

col::Origin *generateBlockOrigin(llvm::BasicBlock &llvmBlock);

col::Origin *generateLabelOrigin(llvm::BasicBlock &llvmBlock);

col::Origin *generateSingleStatementOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateAssignTargetOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateBinExprOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateFunctionCallOrigin(llvm::CallInst &callInstruction);

col::Origin *generateOperandOrigin(llvm::Instruction &llvmInstruction,
                                   llvm::Value &llvmOperand);

col::Origin *
generateGlobalVariableOrigin(llvm::Module &llvmModule,
                             llvm::GlobalVariable &llvmGlobalVariable);

col::Origin *generateGlobalVariableInitializerOrigin(
    llvm::Module &llvmModule, llvm::GlobalVariable &llvmGlobalVariable,
    llvm::Value &llvmInitializer);

col::Origin *generateVoidOperandOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateTypeOrigin(llvm::Type &llvmType);

col::Origin *generateMemoryOrderingOrigin(llvm::AtomicOrdering &llvmOrdering);

std::string extractShortPosition(const col::Origin &origin);

col::Origin *deepenOperandOrigin(const col::Origin &origin,
                                 llvm::Value &llvmOperand);

} // namespace llvm2col
#endif // PALLAS_ORIGINPROVIDER_H
