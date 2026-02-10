#ifndef PALLAS_SPECSTATEMENTTRANSFORM_H
#define PALLAS_SPECSTATEMENTTRANSFORM_H

#include "IRSpec/PallasIRSpec.h"
#include "Passes/Function/FunctionBodyTransformer.h"
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>

/**
 * Implements the transformation of specification statements and other 
 * specifications that are attached to an instruction. 
 */
namespace llvm2col {
namespace col = vct::col::ast;

void transformSpecStmntBlock(llvm::MDNode &llvmSpecBlock,
                             llvm::Instruction &llvmInstr,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &functionCursor);

void transformSpecStmnt(const pallas::irspec::SpecStatement &stmnt,
                        llvm::Instruction &llvmInstr,
                        col::LlvmBasicBlock &colBlock,
                        pallas::FunctionCursor &functionCursor);

void transformGhostAssignBlock(llvm::MDNode &specBlock,
                             llvm::Instruction &llvmInstr,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &functionCursor);

void transformGhostAssign(const pallas::irspec::GhostAssign &gAssign,
                        llvm::Instruction &llvmInstr,
                        col::LlvmBasicBlock &colBlock,
                        pallas::FunctionCursor &functionCursor);


void buildWrapperCall(llvm::Function &wrapperFunction, 
    llvm::ArrayRef<llvm::DILocalVariable *> wrapperArgs, 
    llvm::ArrayRef<llvm::DILocalVariable *> givenArgs,
    llvm::ArrayRef<llvm::DILocalVariable *> yieldsArgs,
    llvm::Instruction &matchedInstruction, 
    const pallas::irspec::SrcLoc &srcLoc,
    col::LlvmFunctionInvocation &colWrapperCall,
    pallas::FunctionCursor &functionCursor);

} // namespace llvm2col

#endif // PALLAS_SPECSTATEMENTTRANSFORM_H
