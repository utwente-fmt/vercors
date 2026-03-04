#include "Transform/Instruction/MemoryOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/BlockUtils.h"
#include <llvm/IR/DebugInfo.h>

const std::string SOURCE_LOC = "Transform::Instruction::MemoryOp";

void llvm2col::transformMemoryOp(llvm::Instruction &llvmInstruction,
                                 col::LlvmBasicBlock &colBlock,
                                 pallas::FunctionCursor &funcCursor) {
    switch (llvm::Instruction::MemoryOps(llvmInstruction.getOpcode())) {
    case llvm::Instruction::Alloca:
        transformAllocA(llvm::cast<llvm::AllocaInst>(llvmInstruction), colBlock,
                        funcCursor);
        break;
    case llvm::Instruction::Load:
        transformLoad(llvm::cast<llvm::LoadInst>(llvmInstruction), colBlock,
                      funcCursor);
        break;
    case llvm::Instruction::Store:
        transformStore(llvm::cast<llvm::StoreInst>(llvmInstruction), colBlock,
                       funcCursor);
        break;
    case llvm::Instruction::GetElementPtr:
        transformGetElementPtr(
            llvm::cast<llvm::GetElementPtrInst>(llvmInstruction), colBlock,
            funcCursor);
        break;
    default:
        reportUnsupportedOperatorError(SOURCE_LOC, llvmInstruction);
    }
}

void llvm2col::transformAllocA(llvm::AllocaInst &allocAInstruction,
                               col::LlvmBasicBlock &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    col::LlvmAllocA *allocA =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_alloc_a();
    allocA->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(allocAInstruction));

    llvm2col::transformAndSetValueType(
        allocAInstruction, allocAInstruction.getAllocatedType(),
        *allocA->mutable_return_type(), 
        getSDResult(funcCursor, allocAInstruction));
    col::Variable &varDecl = funcCursor.declareVariable(
        allocAInstruction, allocAInstruction.getAllocatedType());
    allocA->mutable_variable()->set_id(varDecl.id());

    llvm2col::transformAndSetExpr(funcCursor, allocAInstruction,
                                  *allocAInstruction.getArraySize(),
                                  *allocA->mutable_num_elements());
}

void llvm2col::transformAtomicOrdering(llvm::AtomicOrdering ordering,
                                       col::LlvmMemoryOrdering *colOrdering) {
    switch (ordering) {
    case llvm::AtomicOrdering::NotAtomic:
        colOrdering->mutable_llvm_memory_not_atomic()->set_allocated_origin(
            llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::Unordered:
        colOrdering->mutable_llvm_memory_unordered()->set_allocated_origin(
            llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::Monotonic:
        colOrdering->mutable_llvm_memory_monotonic()->set_allocated_origin(
            llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::Acquire:
        colOrdering->mutable_llvm_memory_acquire()->set_allocated_origin(
            llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::Release:
        colOrdering->mutable_llvm_memory_release()->set_allocated_origin(
            llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::AcquireRelease:
        colOrdering->mutable_llvm_memory_acquire_release()
            ->set_allocated_origin(
                llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    case llvm::AtomicOrdering::SequentiallyConsistent:
        colOrdering->mutable_llvm_memory_sequentially_consistent()
            ->set_allocated_origin(
                llvm2col::generateMemoryOrderingOrigin(ordering));
        break;
    }
}

void llvm2col::transformLoad(llvm::LoadInst &loadInstruction,
                             col::LlvmBasicBlock &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // We are not storing isVolatile and getAlign
    col::LlvmLoad *load =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_load();
    load->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(loadInstruction));
    load->set_allocated_blame(new col::Blame());
    col::Variable &varDecl = funcCursor.declareVariable(loadInstruction);
    load->mutable_variable()->set_id(varDecl.id());
    llvm2col::transformAndSetValueType(
        loadInstruction, nullptr, *load->mutable_load_type(),
        getSDResult(funcCursor, loadInstruction));
    llvm2col::transformAndSetExpr(funcCursor, loadInstruction,
                                  *loadInstruction.getPointerOperand(),
                                  *load->mutable_pointer());
    llvm2col::transformAtomicOrdering(loadInstruction.getOrdering(),
                                      load->mutable_ordering());
}

void llvm2col::transformStore(llvm::StoreInst &storeInstruction,
                              col::LlvmBasicBlock &colBlock,
                              pallas::FunctionCursor &funcCursor) {
    // We are not storing isVolatile and getAlign
    col::LlvmStore *store =
        pallas::bodyAsBlock(colBlock).add_statements()->mutable_llvm_store();
    store->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(storeInstruction));
    store->set_allocated_blame(new col::Blame());
    llvm2col::transformAndSetExpr(funcCursor, storeInstruction,
                                  *storeInstruction.getValueOperand(),
                                  *store->mutable_value());
    llvm2col::transformAndSetExpr(funcCursor, storeInstruction,
                                  *storeInstruction.getPointerOperand(),
                                  *store->mutable_pointer());
    llvm2col::transformAtomicOrdering(storeInstruction.getOrdering(),
                                      store->mutable_ordering());
}

void llvm2col::transformGetElementPtr(llvm::GetElementPtrInst &gepInstruction,
                                      col::LlvmBasicBlock &colBlock,
                                      pallas::FunctionCursor &funcCursor) {

    auto &sdRes = getSDResult(funcCursor, gepInstruction);
    col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
        gepInstruction, colBlock, gepInstruction.getResultElementType());
    col::Expr *gepExpr = assignment.mutable_value();
    col::LlvmGetElementPointer *gep =
        gepExpr->mutable_llvm_get_element_pointer();
    gep->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(gepInstruction));
    gep->set_allocated_blame(new col::Blame());
    // Not using metadata info here since there is basically never a dbg.value
    // or dbg.declare that refers to a GEP
    llvm2col::transformAndSetType(*gepInstruction.getSourceElementType(),
                                  *gep->mutable_structure_type(), sdRes);
    llvm2col::transformAndSetType(*gepInstruction.getResultElementType(),
                                  *gep->mutable_result_type(), sdRes);
    llvm2col::transformAndSetExpr(funcCursor, gepInstruction,
                                  *gepInstruction.getPointerOperand(),
                                  *gep->mutable_pointer());
    for (auto &index : gepInstruction.indices()) {
        llvm2col::transformAndSetExpr(funcCursor, gepInstruction, *index.get(),
                                      *gep->add_indices());
    }
}
