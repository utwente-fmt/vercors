#ifndef PALLAS_OTHEROPTRANSFORM_H
#define PALLAS_OTHEROPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformOtherOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                      pallas::FunctionCursor &funcCursor);
/**
 * Phi nodes get transformed retroactively by creating a variable declaration
 * and retroactively assign the variable in each originating COL block of each
 * phi pair.
 * @param phiInstruction
 * @param funcCursor
 */
void transformPhi(llvm::PHINode &phiInstruction,
                  pallas::FunctionCursor &funcCursor);

void transformICmp(llvm::ICmpInst &icmpInstruction, col::Block &colBlock,
                   pallas::FunctionCursor &funcCursor);
/**
 * Transforms the common part of all compare instructions (the argument pair).
 * Currently only used by transformIcmp but could also be used in the future by
 * for example an FCMP transformation.
 * @param cmpInstruction
 * @param colCompareExpr
 * @param funcCursor
 */
void transformCmpExpr(llvm::CmpInst &cmpInstruction, auto &colCompareExpr,
                      pallas::FunctionCursor &funcCursor);

void transformCallExpr(llvm::CallInst &callInstruction, col::Block &colBlock,
                       pallas::FunctionCursor &funcCursor);

bool checkCallSupport(llvm::CallInst &callInstruction);
} // namespace llvm2col

#endif // PALLAS_OTHEROPTRANSFORM_H
