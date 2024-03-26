#ifndef VCLLVM_ORIGINPROVIDER_H
#define VCLLVM_ORIGINPROVIDER_H

#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include "vct/col/ast/Origin.pb.h"

/**
 * Generators for VerCors origin objects for various LLVM Value types.
 *
 * For more info on VerCors origins see: https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2Col {
    namespace col = vct::col::ast;

    col::Origin *generateProgramOrigin(llvm::Module &llvmModule);

    col::Origin *generateFuncDefOrigin(llvm::Function &llvmFunction);

    col::Origin *generateFunctionContractOrigin(llvm::Function &llvmFunction, const std::string& contract);

    col::Origin *generateGlobalValOrigin(llvm::Module &llvmModule, const std::string &globVal);

    col::Origin *generateArgumentOrigin(llvm::Argument &llvmArgument);

    col::Origin *generateBlockOrigin(llvm::BasicBlock &llvmBlock);

    col::Origin *generateLabelOrigin(llvm::BasicBlock &llvmBlock);

    col::Origin *generateSingleStatementOrigin(llvm::Instruction &llvmInstruction);

    col::Origin *generateAssignTargetOrigin(llvm::Instruction &llvmInstruction);

    col::Origin *generateBinExprOrigin(llvm::Instruction &llvmInstruction);

    col::Origin *generateFunctionCallOrigin(llvm::CallInst &callInstruction);

    col::Origin *generateOperandOrigin(llvm::Instruction &llvmInstruction, llvm::Value &llvmOperand);

    col::Origin *generateTypeOrigin(llvm::Type &llvmType);

}
#endif //VCLLVM_ORIGINPROVIDER_H



