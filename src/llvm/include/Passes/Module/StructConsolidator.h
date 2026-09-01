#ifndef PALLAS_STRUCTCONSOLIDATOR_H
#define PALLAS_STRUCTCONSOLIDATOR_H

#include "IRSpec/PallasIRSpec.h"
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>

namespace pallas {
using namespace llvm;

class StructConsolidatorPass : public PassInfoMixin<StructConsolidatorPass> {
  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);

  private:
    struct Write {
        uint64_t Offset;
        uint64_t Size;
        Value *Src;
        Instruction *WriteI;
    };

    struct ArgInfo {
        Argument *Arg;
        uint64_t Offset;
        uint64_t Size;
    };

    using FieldMap = DenseMap<uint64_t, std::pair<size_t, Value *>>;

    struct CallInfo {
        FieldMap Fields;
        AllocaInst *Intermediary;
        MDNode *StmntBlock;
    };

    struct ReplaceableArgSet {
        SmallVector<ArgInfo> Arguments;
        AllocaInst *Alloc;
        AllocaInst *Intermediary;
        DenseMap<CallBase *, CallInfo> Calls;
        bool Valid;
        // Writes that initialize the alloca
        SmallVector<Instruction *> Writes;
    };

    struct Fail {};
    struct Found {};
    struct FoundAll {
        AllocaInst *Intermediary;
    };
    using DigToFieldResult = std::variant<Fail, Found, FoundAll>;

    using WriteVec = SmallVector<Write>;
    using AllocaMap = DenseMap<AllocaInst *, WriteVec>;
    using ReplaceableVec = SmallVector<ReplaceableArgSet>;

    // Set to track the alloca-instructions that were inserted on the call-site 
    // of functions with consolidated arguments. 
    // Required to prevent the inserted allocas to be consolidated as well. 
    SmallPtrSet<AllocaInst *, 8> CallSiteAllocas;

    void removeRecursively(Value *V, SmallSet<Value *, 8> &Visited);
    void removeParentless(Value *V);
    DigToFieldResult digToField(const Function &F, Value *V,
                                const DataLayout &L, StructType &ST,
                                FieldMap &Fields, ArgInfo &A,
                                APInt SourceOffset, uint64_t FieldOffset,
                                size_t Depth, MDNode **StmntBlock);
    void gatherUseData(const Function &F, const DataLayout &L,
                       ReplaceableArgSet &Set);
    bool gatherWrites(const Function &F, const DataLayout &L, uint64_t Size,
                      const Value &V, APInt Offset, WriteVec &Writes,
                      SmallVectorImpl<Instruction *> &LaterWrites);
    /**
     * Replace all references in specification-metadata that point to
     * wrapper-function WF with references to function NWF.
     */
    void replaceWrapperReferences(Function &WF, Function &NWF);

    /**
     * If the given offset maps to the start of an element in the given
     * struct type, return the index of the element. Otherwise return nullopt.
     */
    std::optional<unsigned>
    getStructElemAtOffset(StructType &S, uint64_t Offset, const DataLayout &L);

    /**
     * Find users of the argument that are not part of the provided list
     * of writes.
     */
    SmallSet<llvm::User *, 8>
    getDirectUsers(Argument &Arg, const SmallVector<Instruction *> &Writes);

    const Function &updateFunction(Function &F, const ReplaceableVec &Sets,
                                   const DataLayout &L);
    void replaceFunctionUse(CallInst *Call, const Function &OldF,
                            Function *NewF, const ReplaceableVec &Sets);
    ReplaceableVec findReplaceableSets(Function &F, const DataLayout &L,
                                       const DominatorTree &DT);
    /**
     * If the spec-element references wrapper-function OWF,
     * replaces the reference to the wrapper-function in the underlying MDNode
     * with a reference to NWF.
     *
     * NOTE: This means that other existing instances of WrappedSpecElement go
     * out-of-sync, i.e. still point to the old wrapper function.
     */
    void replaceWrapper(irspec::WrappedSpecElement &S, llvm::Function &OWF,
                        llvm::Function &NWF);
};

} // namespace pallas

#endif