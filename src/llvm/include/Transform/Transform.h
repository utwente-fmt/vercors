#ifndef PALLAS_TRANSFORM_H
#define PALLAS_TRANSFORM_H

#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionBodyTransformer.h"

/**
 * General helper functions for transformations
 */

namespace llvm2col {
namespace col = vct::col::ast;

// type transformers
void transformAndSetPointerType(llvm::Type &llvmType, col::Type &colType);

void transformAndSetType(llvm::Type &llvmType, col::Type &colType);

/**
 * ATTEMPTS to convert any integer constant to a BigInt representation.
 * @param apInt
 * @param colIntegerValue
 */
void transformAndSetBigInt(llvm::APInt &apInt, col::BigInt &bigInt);

/**
 * Transforms and set LLVM expression in the buffer which in practice are either
 * constants (e.g. 0, 0.1, false etc..) or variables (i.e. LLVM Values) (e.g.
 * %3, %variable)
 * @param functionCursor
 * @param llvmInstruction
 * @param llvmOperand
 * @param colExpr
 */
void transformAndSetExpr(pallas::FunctionCursor &functionCursor,
                         llvm::Instruction &llvmInstruction,
                         llvm::Value &llvmOperand, col::Expr &colExpr);
/**
 * Used by TransformAndSetExpr
 * @param llvmInstruction
 * @param llvmConstant
 * @param colExpr
 */
void transformAndSetConstExpr(llvm::FunctionAnalysisManager &FAM,
                              col::Origin *origin, llvm::Constant &llvmConstant,
                              col::Expr &colExpr);

/**
 * Used by TransformAndSetExpr
 * @param functionCursor
 * @param llvmInstruction
 * @param llvmOperand
 * @param colExpr
 */
void transformAndSetVarExpr(pallas::FunctionCursor &functionCursor,
                            col::Origin *origin, bool inPhiNode,
                            llvm::Value &llvmOperand, col::Expr &colExpr);
template <class ColBinExpr>
void transformBinExpr(llvm::Instruction &llvmInstruction,
                      ColBinExpr &colBinExpr,
                      pallas::FunctionCursor &funcCursor) {
    // set origin of entire expression
    colBinExpr.set_allocated_origin(generateBinExprOrigin(llvmInstruction));
    // transform left operand
    col::Expr *lExpr = colBinExpr.mutable_left();
    llvm2col::transformAndSetExpr(funcCursor, llvmInstruction,
                                  *llvmInstruction.getOperand(0), *lExpr);
    // transform right operand
    col::Expr *rExpr = colBinExpr.mutable_right();
    llvm2col::transformAndSetExpr(funcCursor, llvmInstruction,
                                  *llvmInstruction.getOperand(1), *rExpr);
}

template <class IDNode> int64_t setColNodeId(IDNode &idNode) {
    auto id = reinterpret_cast<int64_t>(idNode);
    idNode->set_id(id);
    return id;
}
/**
 * Returns a string representation of any LLVM value as it would be displayed in
 * human readable LLVM IR
 * @param llvmValue
 * @return
 */
std::string getValueName(llvm::Value &llvmValue);
} // namespace llvm2col
#endif // PALLAS_TRANSFORM_H
