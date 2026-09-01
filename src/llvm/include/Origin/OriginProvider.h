#ifndef PALLAS_ORIGINPROVIDER_H
#define PALLAS_ORIGINPROVIDER_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/Origin.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
#include "IRSpec/PallasIRSpec.h"
#include <llvm/Analysis/LoopInfo.h>
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

col::Origin *generateLabelledOrigin(const std::string label);

col::Origin *generateProgramOrigin(llvm::Module &llvmModule);

col::Origin *generateFuncDefOrigin(llvm::Function &llvmFunction);

col::Origin *generateFunctionContractOrigin(llvm::Function &llvmFunction,
                                            const std::string &contract);

col::Origin *generateGlobalValOrigin(llvm::Module &llvmModule,
                                     const std::string &globVal);

col::Origin *generateArgumentOrigin(llvm::Argument &llvmArgument);

col::Origin *generateBlockOrigin(llvm::BasicBlock &llvmBlock);

col::Origin *generateLabelOrigin(llvm::BasicBlock &llvmBlock);

col::Origin *generateLoopOrigin(llvm::Loop &llvmLoop);

/**
 * Generates an origin for a pallas function-contract.
 */
col::Origin *
generatePallasFunctionContractOrigin(const llvm::Function &f,
                                     const pallas::irspec::SrcLoc &loc);

col::Origin *
generatePallasLoopContractOrigin(const llvm::Loop &loop,
                                 const pallas::irspec::SrcLoc &loc);

col::Origin *generateSingleStatementOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateAssignTargetOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateBinExprOrigin(llvm::Instruction &llvmInstruction);

col::Origin *generateFunctionCallOrigin(llvm::CallInst &callInstruction);

/**
 * Generates an origin for generated call to a wrapper function of the clause
 * of a pallas function-contract.
 */
col::Origin *
generatePallasWrapperCallOrigin(const llvm::Function &wrapperFunc,
                                const pallas::irspec::SrcLoc &clauseSrcLoc);

/**
 * Generates an origin for generated call to a wrapper function of a
 * specification element.
 */
col::Origin *generatePallasWrapperCallOrigin(
    const pallas::irspec::WrappedSpecElement &specElem);

/**
 * Generates an origin for a clause of a pallas function contract that is
 * attached to the given function. Assumes that the provided metadata-node is a
 * well-formed encoding of a source-location (adhering to the location-format of
 * pallas).
 */
col::Origin *
generatePallasFContractClauseOrigin(const llvm::Function &parentFunc,
                                    const pallas::irspec::SrcLoc &clauseSrcLoc,
                                    unsigned int clauseNum);

col::Origin *generatePallasSpecStmntOrigin(const llvm::Instruction &llvmInstr,
                                           const pallas::irspec::SrcLoc &loc,
                                           const std::string &stmntType);

/**
 * Generates an origin based on a source-location in the format of
 * Pallas and with the given preferred name.
 */
col::Origin *generatePallasSpecOrigin(const pallas::irspec::SrcLoc &srcLoc,
                                      const std::string &preferedName);

/**
 * Adds the source-location that is encoded by specification format of Pallas
 * to the given origin.
 */
void addSourceLocFromPallasLoc(col::Origin *origin,
                               const pallas::irspec::SrcLoc &loc);

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

col::Origin *generateDITypeOrigin(llvm::DIType &debugType);

col::Origin *generateStructMemberOrigin(llvm::DIDerivedType &debugType);

col::Origin *generateMemoryOrderingOrigin(llvm::AtomicOrdering &llvmOrdering);

std::string extractShortPosition(const col::Origin &origin);

col::Origin *deepenOperandOrigin(const col::Origin &origin,
                                 llvm::Value &llvmOperand);

} // namespace llvm2col
#endif // PALLAS_ORIGINPROVIDER_H
