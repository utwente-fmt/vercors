#ifndef PALLAS_STRUCTCONSOLIDATOR_H
#define PALLAS_MODULESPECCOLLECTOR_H

#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/DataLayout.h>
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
    };

    struct ArgInfo {
        const Argument *Arg;
        uint64_t Offset;
        uint64_t Size;
    };

    using FieldMap = DenseMap<uint64_t, std::pair<size_t, Value *>>;
    struct ReplaceableArgSet {
        SmallVector<ArgInfo> Arguments;
        AllocaInst *Alloc;
        AllocaInst *Intermediary;
        DenseMap<CallBase *, std::pair<FieldMap, AllocaInst *>> Calls;
        bool Valid;
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

    void removeRecursively(Value *V, SmallSet<Value *, 8> &Visited);
    void removeParentless(Value *V);
    DigToFieldResult digToField(const Function &F, Value *V,
                                const DataLayout &L, StructType &ST,
                                FieldMap &Fields, ArgInfo &A,
                                APInt SourceOffset, uint64_t FieldOffset,
                                size_t Depth);
    void gatherUseData(const Function &F, const DataLayout &L,
                       ReplaceableArgSet &Set);
    bool gatherWrites(const Function &F, const DataLayout &L, uint64_t Size,
                      const Value &V, APInt Offset, WriteVec &Writes);
    void replaceWrapperCalls(Function *F, Function *NF,
                             SmallSet<const Argument *, 8> &ToBeRemoved,
                             MDNode *MD, SmallSet<MDNode *, 8> &Visited);
    const Function &updateFunction(Function &F, const ReplaceableVec &Sets);
    void replaceFunctionUse(CallInst *Call, const Function &OldF,
                            Function *NewF, const ReplaceableVec &Sets);
    ReplaceableVec findReplaceableSets(Function &F, const DataLayout &L);
};

} // namespace pallas

#endif