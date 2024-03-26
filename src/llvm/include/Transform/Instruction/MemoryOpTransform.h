#ifndef PALLAS_MEMORYOPTRANSFORM_H
#define PALLAS_MEMORYOPTRANSFORM_H
#include "Passes/Function/FunctionBodyTransformer.h"

namespace llvm2col {
namespace col = vct::col::ast;

void transformMemoryOp(llvm::Instruction &llvmInstruction, col::Block &colBlock,
                       pallas::FunctionCursor &funcCursor);

void transformAllocA(llvm::AllocaInst &allocAInstruction, col::Block &colBlock,
                     pallas::FunctionCursor &funcCursor);

void transformAtomicOrdering(llvm::AtomicOrdering ordering,
                             col::LlvmMemoryOrdering *colOrdering);

void transformLoad(llvm::LoadInst &loadInstruction, col::Block &colBlock,
                   pallas::FunctionCursor &funcCursor);

void transformStore(llvm::StoreInst &storeInstruction, col::Block &colBlock,
                    pallas::FunctionCursor &funcCursor);

void transformGetElementPtr(llvm::GetElementPtrInst &gepInstruction,
                            col::Block &colBlock,
                            pallas::FunctionCursor &funcCursor);
} // namespace llvm2col
#endif // PALLAS_MEMORYOPTRANSFORM_H
