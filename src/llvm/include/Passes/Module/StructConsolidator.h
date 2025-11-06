#ifndef PALLAS_STRUCTCONSOLIDATOR_H
#define PALLAS_MODULESPECCOLLECTOR_H

#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/PassManager.h>

namespace pallas {
using namespace llvm;

class StructConsolidatorPass : public PassInfoMixin<StructConsolidatorPass> {
    public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
    private:
    struct Write {
        uint64_t offset;
        uint64_t size;
        Value *src;
    };

    struct ArgInfo {
        const Argument * argument;
        uint64_t offset;
        uint64_t size;
    };

    using FieldMap = DenseMap<uint64_t, std::pair<size_t, Value *>>;
    struct ReplaceableArgSet {
        SmallVector<ArgInfo> arguments;
        AllocaInst *alloc;
        AllocaInst *intermediary;
        DenseMap<CallBase *, FieldMap> calls;
        bool valid;
    };

    using WriteVec = SmallVector<Write>;
    using AllocaMap = DenseMap<AllocaInst *, WriteVec>;
    using ReplaceableVec = SmallVector<ReplaceableArgSet>;

    void removeRecursively(Value *V);
    void removeParentless(Value *V);
    bool digToField(Value *V, const DataLayout &L, const StructType &structType, FieldMap &fields, ArgInfo &A, APInt offsetIntoSource, uint64_t offsetIntoField, size_t pointerDepth);
    void gatherUseData(const Function &F, const DataLayout &L, ReplaceableArgSet &set);
    bool gatherWrites(const Function &F, const DataLayout &L, uint64_t typeSize, const Value &value, APInt currentOffset, WriteVec &writes);
    const Function &updateFunction(Function &F, const ReplaceableVec &sets);
    void replaceFunctionUse(CallInst *call, const Function &oldF, Function *newF, const ReplaceableVec &sets);
    ReplaceableVec findReplaceableSets(Function &F, const DataLayout &L);
};

}

#endif