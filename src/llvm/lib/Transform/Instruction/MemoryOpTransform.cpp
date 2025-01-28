#include "Transform/Instruction/MemoryOpTransform.h"

#include "Origin/OriginProvider.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"
#include <llvm/IR/DebugInfo.h>

const std::string SOURCE_LOC = "Transform::Instruction::MemoryOp";

void llvm2col::transformMemoryOp(llvm::Instruction &llvmInstruction,
                                 col::Block &colBlock,
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
                               col::Block &colBlock,
                               pallas::FunctionCursor &funcCursor) {
    col::LlvmAllocA *allocA = colBlock.add_statements()->mutable_llvm_alloc_a();
    allocA->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(allocAInstruction));

    if (allocAInstruction.getAllocatedType()->getTypeID() ==
        llvm::Type::PointerTyID) {
        // Pointers are opaque so we'll use the metadata to try and figure out
        // what this pointer will point to
        for (llvm::DbgDeclareInst *dbg :
             llvm::FindDbgDeclareUses(&allocAInstruction)) {
            llvm::errs() << "Use of AllocA ptr " << *dbg << "\n";
            llvm::CallInst *dbgCall = llvm::cast<llvm::CallInst>(dbg);
            llvm::Metadata *metadata =
                llvm::cast<llvm::MetadataAsValue>(dbgCall->getOperand(1))
                    ->getMetadata();
            // TODO: Translate this information where possible
        }
        llvm2col::transformAndSetType(*allocAInstruction.getAllocatedType(),
                                      *allocA->mutable_allocation_type());
    } else {
        llvm2col::transformAndSetType(*allocAInstruction.getAllocatedType(),
                                      *allocA->mutable_allocation_type());
    }
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
                             col::Block &colBlock,
                             pallas::FunctionCursor &funcCursor) {
    // We are not storing isVolatile and getAlign
    col::LlvmLoad *load = colBlock.add_statements()->mutable_llvm_load();
    load->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(loadInstruction));
    load->set_allocated_blame(new col::Blame());
    col::Variable &varDecl = funcCursor.declareVariable(loadInstruction);
    load->mutable_variable()->set_id(varDecl.id());
    llvm2col::transformAndSetType(*loadInstruction.getType(),
                                  *load->mutable_load_type());
    llvm2col::transformAndSetExpr(funcCursor, loadInstruction,
                                  *loadInstruction.getPointerOperand(),
                                  *load->mutable_pointer());
    llvm2col::transformAtomicOrdering(loadInstruction.getOrdering(),
                                      load->mutable_ordering());
}

void llvm2col::transformStore(llvm::StoreInst &storeInstruction,
                              col::Block &colBlock,
                              pallas::FunctionCursor &funcCursor) {
    // We are not storing isVolatile and getAlign
    col::LlvmStore *store = colBlock.add_statements()->mutable_llvm_store();
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
                                      col::Block &colBlock,
                                      pallas::FunctionCursor &funcCursor) {

    col::Assign &assignment = funcCursor.createAssignmentAndDeclaration(
        gepInstruction, colBlock, gepInstruction.getResultElementType());
    col::Expr *gepExpr = assignment.mutable_value();
    col::LlvmGetElementPointer *gep =
        gepExpr->mutable_llvm_get_element_pointer();
    gep->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(gepInstruction));
    llvm2col::transformAndSetType(*gepInstruction.getSourceElementType(),
                                  *gep->mutable_structure_type());
    llvm2col::transformAndSetType(*gepInstruction.getResultElementType(),
                                  *gep->mutable_result_type());
    llvm2col::transformAndSetExpr(funcCursor, gepInstruction,
                                  *gepInstruction.getPointerOperand(),
                                  *gep->mutable_pointer());
    for (auto &index : gepInstruction.indices()) {
        llvm2col::transformAndSetExpr(funcCursor, gepInstruction, *index.get(),
                                      *gep->add_indices());
    }
}
