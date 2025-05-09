#ifndef PALLAS_INTRINSICSTRANSFORM_H
#define PALLAS_INTRINSICSTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

#include <llvm/IR/Instructions.h>

namespace llvm2col {
namespace col = vct::col::ast;

void transformIntrinsic(llvm::CallInst &callInstruction,
                        col::LlvmBasicBlock &colBlock,
                        pallas::FunctionCursor &funcCursor);

/**
 * Transform call to the @llvm.trap()-intrinsic
 */
void transformTrap(llvm::CallInst &callInstruction,
                   col::LlvmBasicBlock &colBlock,
                   pallas::FunctionCursor &funcCursor);

/**
 * Transform call to the @llvm.expect.XXX-intrinsic
 */
void transformExpect(llvm::CallInst &callInstruction,
                     col::LlvmBasicBlock &colBlock,
                     pallas::FunctionCursor &funcCursor);

/**
 * Transform call to the @llvm.sadd.with.overflow or
 *                       @llvm.uadd.with.overflow-intrinsic
 */
void transformAddWithOverflow(llvm::CallInst &callInstruction,
                              col::LlvmBasicBlock &colBlock,
                              pallas::FunctionCursor &funcCursor, bool sign);

/**
 * Transform call to the @llvm.ssub.with.overflow or
 *                       @llvm.usub.with.overflow-intrinsic
 */
void transformSubWithOverflow(llvm::CallInst &callInstruction,
                              col::LlvmBasicBlock &colBlock,
                              pallas::FunctionCursor &funcCursor, bool sign);

/**
 * Transform call to the @llvm.smult.with.overflow or
 *                       @llvm.umult.with.overflow-intrinsic
 */
void transformMultWithOverflow(llvm::CallInst &callInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor, bool sign);

/**
 * Transform call to the @llvm.memset.*-intrinsic
 */
void transformMemset(llvm::CallInst &callInstruction,
                     col::LlvmBasicBlock &colBlock,
                     pallas::FunctionCursor &funcCursor);

} // namespace llvm2col

#endif // PALLAS_INTRINSICSTRANSFORM_H
