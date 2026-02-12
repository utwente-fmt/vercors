#include "Passes/Module/StructConsolidator.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"
#include <algorithm>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Operator.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Local.h>
#include <variant>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::StructConsolidator";

using namespace llvm;

struct Interval {
    uint64_t Start;
    uint64_t End;
};

struct IntervalSet {
    SmallVector<Interval> Intervals;

    void add(uint64_t Start, uint64_t End) {
        assert(Start < End);
        if (Intervals.empty()) {
            Intervals.push_back({Start, End});
        } else {
            for (size_t Idx = 0, Size = Intervals.size(); Idx < Size; ++Idx) {
                if (Intervals[Idx].Start > Start) {
                    if (Intervals[Idx].Start <= End) {
                        Intervals[Idx].Start = Start;
                        Intervals[Idx].End = std::max(Intervals[Idx].End, End);
                    } else {
                        Intervals.insert(Intervals.begin() + Idx, {Start, End});
                    }
                } else {
                    if (Intervals[Idx].End <= Start) {
                        Intervals[Idx].End = std::max(Intervals[Idx].End, End);
                    } else if (Idx == End - 1) {
                        Intervals.push_back({Start, End});
                    }
                }
            }
        }
    }

    bool contains(uint64_t Start, uint64_t End) {
        for (const Interval &I : Intervals) {
            if (I.Start < Start) {
                if (End <= I.End) {
                    return true;
                }
            } else {
                return I.Start == Start && End <= I.End;
            }
        }
        return false;
    }
};

// WARNING: This can remove a lot of things, be very careful when calling this
void StructConsolidatorPass::removeRecursively(Value *V,
                                               SmallSet<Value *, 8> &Visited) {
    if (!Visited.insert(V).second)
        return;
    while (!V->use_empty()) {
        removeRecursively(V->user_back(), Visited);
    }
    if (auto *I = dyn_cast<Instruction>(V)) {
        salvageDebugInfo(*I);
        for (Use &U : I->operands()) {
            Value *OpV = U.get();
            U.set(nullptr);

            if (!OpV->use_empty())
                continue;

            removeRecursively(OpV, Visited);
        }
        I->eraseFromParent();
    }
}

void StructConsolidatorPass::removeParentless(Value *V) {
    if (auto *I = dyn_cast<Instruction>(V)) {
        if (I->getParent())
            return;
        for (auto &O : I->operands()) {
            removeParentless(O);
        }
        V->deleteValue();
    }
}

StructConsolidatorPass::DigToFieldResult StructConsolidatorPass::digToField(
    const Function &F, Value *V, const DataLayout &L, StructType &ST,
    FieldMap &Fields, ArgInfo &A, APInt SourceOffset, uint64_t FieldOffset,
    size_t Depth, MDNode **StmntBlock, MDNode **GhostAssignBlock) {
    // For now let's not consider deeper nesting
    if (Depth > 1)
        return Fail{};

    // Check if V has any specifications attached to it
    // (No need to check for given/yields bindings because these are only
    // attached to calls.)
    if (auto *I = dyn_cast<Instruction>(V)) {
        if (MDNode *Block = utils::getSpecStmntBlock(*I)) {
            if (*StmntBlock != nullptr && *StmntBlock != Block) {
                ErrorReporter::addWarning(
                    SOURCE_LOC,
                    "Multiple StmntBlock annotations in argument building for "
                    "call, we won't consolidate this struct",
                    *I);
                return Fail{};
            }
            *StmntBlock = Block;
        }
        if (MDNode *Block = irspec::getGhostAssignBlockMD(*I)) {
            if (*GhostAssignBlock != nullptr && *GhostAssignBlock != Block) {
                ErrorReporter::addWarning(
                    SOURCE_LOC,
                    "Multiple Ghost assignment block annotations in argument "
                    "building for call, we won't consolidate this struct",
                    *I);
                return Fail{};
            }
            *GhostAssignBlock = Block;
        }
    }

    auto &[Idx, Field] = Fields[A.Offset + FieldOffset];
    Type *ET = ST.getStructElementType(Idx);
    // We only want to find one source for each field
    if (Field != NULL)
        return Fail{};
    if (Depth == 0 && V->getType() == ET) {
        // We found a good source!
        Field = V;
        return Found{};
    }

    if (auto *Load = dyn_cast<LoadInst>(V)) {
        return digToField(F, Load->getPointerOperand(), L, ST, Fields, A,
                          SourceOffset, FieldOffset, Depth + 1, StmntBlock,
                          GhostAssignBlock);
    }

    if (auto *GEP = dyn_cast<GetElementPtrInst>(V)) {
        if (!GEP->accumulateConstantOffset(L, SourceOffset))
            return Fail{};

        return digToField(F, GEP->getPointerOperand(), L, ST, Fields, A,
                          SourceOffset, FieldOffset, Depth, StmntBlock,
                          GhostAssignBlock);
    }

    auto *AllocA = dyn_cast<AllocaInst>(V);

    if (!AllocA)
        return Fail{};

    assert(Depth == 1);

    if (ST.getElementType(Idx) == AllocA->getAllocatedType()) {
        // This is our source, we just need to load it
        // Byte-align is fine since we're never generating this code
        Field = new LoadInst(ET, V, Twine("insertedLoad"), false, Align());
        return Found{};
    }

    if (!isa<StructType>(AllocA->getAllocatedType())) {
        // While this could technically be an intermediary there should be
        // no need to generate it like that since you could have a direct
        // Load instruction
        return Fail{};
    }

    StructType *AllocST = cast<StructType>(AllocA->getAllocatedType());
    const StructLayout *structLayout = L.getStructLayout(AllocST);

    // Decompose into available fields!
    // We have A.size bytes that we are reading from this allocation
    // We will get all fields starting from field[A.offset +
    // offsetIntoField]
    bool IsIntermediary = false;
    int64_t Remaining = A.Size;
    while (Remaining > 0) {
        auto &[InnerIdx, InnerField] = Fields[A.Offset + FieldOffset];
        assert(InnerField == NULL);
        int sourceIndex = structLayout->getElementContainingOffset(
            SourceOffset.getLimitedValue());

        // Somehow we've ended up misaligned somewhere
        if (SourceOffset != structLayout->getElementOffset(sourceIndex))
            return Fail{};

        if (ST.getStructElementType(InnerIdx) !=
            AllocST->getStructElementType(sourceIndex)) {
            // This must be an intermediary struct
            IsIntermediary = true;
            break;
        }

        // Found a match
        InnerField = new LoadInst(
            ET,
            GetElementPtrInst::Create(
                AllocST, V,
                ArrayRef(
                    new Value *[]{
                        ConstantInt::get(ST.getContext(), APInt(32, 0)),
                        ConstantInt::get(ST.getContext(), APInt(32, InnerIdx))},
                    2)),
            Twine("insertedLoad"), false, Align());
        const TypeSize FieldSize =
            L.getTypeAllocSize(ST.getStructElementType(InnerIdx));
        SourceOffset += FieldSize;
        FieldOffset += FieldSize.getFixedValue();
        Remaining -= FieldSize.getFixedValue();
    }

    if (!IsIntermediary) {
        assert(Remaining == 0);
        return Found{};
    }

    // We only support memcpys of the whole struct (this is easily extendable to
    // more cases in the future)
    if (FieldOffset != 0 || !SourceOffset.isZero()) {
        std::string M;
        {
            raw_string_ostream S(M);
            S << "Not simplifying function `";
            F.printAsOperand(S, true, F.getParent());
            S << "`, because caller uses intermediary `";
            AllocA->printAsOperand(S, true);
            S << "` at offset (we only support whole array copies)";
        }
        ErrorReporter::addWarning(SOURCE_LOC, M);
        return Fail{};
    }

    // Find memcpy
    bool Found = false;
    for (const Use &U : AllocA->uses()) {
        if (auto *GEP = dyn_cast<GetElementPtrInst>(U.getUser())) {
            for (const User *GEPUser : GEP->users()) {
                if (!isa<LoadInst>(GEPUser) &&
                    (!isa<CallInst>(GEPUser) ||
                     cast<CallInst>(GEPUser)->getCalledFunction() != &F)) {
                    std::string M;
                    {
                        raw_string_ostream S(M);
                        S << "Not simplifying function `";
                        F.printAsOperand(S, true, F.getParent());
                        S << "`, because caller uses intermediary `";
                        AllocA->printAsOperand(S, true);
                        S << "` with unexpected instruction: `";
                        GEPUser->print(S);
                        S << "`";
                    }
                    ErrorReporter::addWarning(SOURCE_LOC, M);
                    return Fail{};
                }
            }
            continue;
        }

        auto *Call = dyn_cast<CallInst>(U.getUser());
        if (!Call) {
            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not simplifying function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because caller uses intermediary `";
                AllocA->printAsOperand(S, true);
                S << "` with unexpected instruction: `";
                U.getUser()->print(S);
                S << "`";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            return Fail{};
        }
        if (Found) {
            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not simplifying function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because caller writes to intermediary  `";
                AllocA->printAsOperand(S, true);
                S << "` more than once";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            return Fail{};
        }

        // Check for memcpy
        Function *IF = Call->getCalledFunction();
        // If we are calling a spec lib function then it will not have
        // side-effects
        if (IF->hasMetadata(constants::PALLAS_SPEC_LIB_MARKER))
            continue;

        if (IF->getIntrinsicID() != Intrinsic::memcpy)
            return Fail{};

        // Check if we are the destination (otherwise there is an additional
        // read which we cannot simplify away)
        if (U.getOperandNo() != 0) {
            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not simplifying function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because caller uses intermediary `";
                AllocA->printAsOperand(S, true);
                S << "` in unexpected way: `";
                Call->print(S);
                S << "`";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            return Fail{};
        }

        Value *Src = Call->getArgOperand(1);
        // Expecting src is an alloca of a struct with the same size as our
        // struct
        if (!isa<AllocaInst>(Src)) {
            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not simplifying function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because caller writes to intermediary `";
                AllocA->printAsOperand(S, true);
                S << "` from an invalid source/offset: `";
                Call->print(S);
                S << "`";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            return Fail{};
        }

        AllocaInst *SrcI = cast<AllocaInst>(Src);
        auto SrcSize = SrcI->getAllocationSize(L);
        Value *Length = Call->getArgOperand(2);
        const TypeSize StructSize = L.getTypeAllocSize(&ST);
        // Expecting Length is an integer equal to the size of our struct
        if (!SrcSize.has_value() || *SrcSize < StructSize ||
            !isa<ConstantInt>(Length) ||
            !cast<ConstantInt>(Length)->equalsInt(StructSize)) {
            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not simplifying function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because caller writes to intermediary `";
                AllocA->printAsOperand(S, true);
                S << "` with an invalid size: `";
                Call->print(S);
                S << "`";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            return Fail{};
        }

        for (auto &[_Offset, IndexField] : Fields) {
            auto &[FieldIdx, FieldGetter] = IndexField;
            if (FieldGetter != nullptr) {
                std::string M;
                {
                    raw_string_ostream S(M);
                    S << "Not simplifying function `";
                    F.printAsOperand(S, true, F.getParent());
                    S << "`, because caller mixes intermediary  `";
                    AllocA->printAsOperand(S, true);
                    S << "` and other origins";
                }
                ErrorReporter::addWarning(SOURCE_LOC, M);
                return Fail{};
            }

            FieldGetter = new LoadInst(
                ET,
                GetElementPtrInst::Create(
                    &ST, SrcI,
                    ArrayRef(
                        new Value *[]{
                            ConstantInt::get(ST.getContext(), APInt(32, 0)),
                            ConstantInt::get(ST.getContext(),
                                             APInt(32, FieldIdx))},
                        2)),
                Twine("insertedLoad"), false, Align());
        }
        Found = true;
    }

    // TODO: Maybe error message on no memcpy found?
    if (Found) {
        return FoundAll{cast<AllocaInst>(V)};
    } else {
        return Fail{};
    }
}

void StructConsolidatorPass::gatherUseData(const Function &F,
                                           const DataLayout &L,
                                           ReplaceableArgSet &Set) {
    auto *ST = cast<StructType>(Set.Alloc->getAllocatedType());
    const StructLayout *SL = L.getStructLayout(ST);
    const auto Offsets = SL->getMemberOffsets();
    AllocaInst *Intermediary = nullptr;
    for (const Use &U : F.uses()) {
        FieldMap Fields(Offsets.size());
        for (size_t Idx = 0, E = Offsets.size(); Idx < E; ++Idx) {
            Fields.insert({Offsets[Idx].getFixedValue(), {Idx, nullptr}});
        }

        if (!isa<CallInst>(U.getUser())) {
            // Copy of logic from Function::hasAddressTaken
            const User *FUU = U.getUser();
            if (isa<BitCastOperator, AddrSpaceCastOperator>(U) &&
                FUU->hasOneUse() && !FUU->user_begin()->user_empty())
                FUU = *FUU->user_begin();
            if (llvm::all_of(FUU->users(), [](const User *U) {
                    if (const auto *GV = dyn_cast<GlobalVariable>(U))
                        return GV->hasName() &&
                               (GV->getName() == "llvm.compiler.used" ||
                                GV->getName() == "llvm.used");
                    return false;
                }))
                return;

            std::string M;
            {
                raw_string_ostream S(M);
                S << "Not including function `";
                F.printAsOperand(S, true, F.getParent());
                S << "`, because it is used in `";
                U.getUser()->print(S);
                S << "`";
            }
            ErrorReporter::addWarning(SOURCE_LOC, M);
            Set.Valid = false;
            return;
        }

        auto *Call = cast<CallInst>(U.getUser());
        if (Call->getCalledFunction() != &F || Call->hasOperandBundles()) {
            Set.Valid = false;
            return;
        }
        MDNode *StmntBlock = nullptr;
        MDNode *GhostAssignBlock = nullptr;
        const Use *P = Call->arg_begin();
        // Find operands for every arg in set.arguments
        for (const Argument *FA = F.arg_begin(), *E = F.arg_end(); FA != E;
             ++FA, ++P) {
            DigToFieldResult Result = Fail{};
            for (ArgInfo &SA : Set.Arguments) {
                if (FA != SA.Arg) {
                    continue;
                }
                Result = digToField(
                    F, P->get(), L, *ST, Fields, SA,
                    APInt(L.getPointerSizeInBits(F.getAddressSpace()), 0,
                          false),
                    0, 0, &StmntBlock, &GhostAssignBlock);

                if (!std::holds_alternative<Fail>(Result)) {
                    break;
                }
                Set.Valid = false;
                for (auto &[_Key, Value] : Fields) {
                    auto &[_Idx, Field] = Value;
                    if (Field != NULL && isa<Instruction>(Field)) {
                        removeParentless(cast<Instruction>(Field));
                    }
                }
                return;
            }

            if (std::holds_alternative<Fail>(Result)) {
                Set.Valid = false;
                return;
            } else if (auto FA = std::get_if<FoundAll>(&Result)) {
                Intermediary = FA->Intermediary;
                break;
            } else {
                assert(std::holds_alternative<Found>(Result));
            }
        }
        // For each operand move up until we find a variable of type
        // set.alloc->getAllocationType If not found bail
        //  (alternatively we can look to see if we can find a value the matches
        //  the size / offset of the fields corresponding to the arg) (For
        //  example: %1 = alloca {i32, i32}; %2 = 10; %3 = load i64, ptr %1,
        //  align 4; call void f(%3, %2))
        // If found store origin (and intermediary), we'll allow multiple
        // origins as long as they're all of the appropriate type
        Set.Calls.insert(
            {Call, {Fields, Intermediary, StmntBlock, GhostAssignBlock}});
    }
}

void StructConsolidatorPass::replaceFunctionUse(
    CallInst *Call, const Function &OldF, Function *NewF,
    DenseMap<const Argument *, const ReplaceableArgSet *> ArgMapping) {

    SmallSet<size_t, 8> ToBeRemoved;
    SmallSet<const ReplaceableArgSet *, 4> Sets;
    DenseMap<size_t, const ReplaceableArgSet *> OldIdxToSet;
    for (auto [A, Set] : ArgMapping) {
        auto OldIdx = A->getArgNo();
        ToBeRemoved.insert(OldIdx);
        OldIdxToSet[OldIdx] = Set;
        Sets.insert(Set);
    }

    MDNode *StmntBlock = utils::getSpecStmntBlock(*Call);
    MDNode *GhostAssignBlock = irspec::getGhostAssignBlockMD(*Call);

    std::vector<Value *> NewArgs;
    SmallSet<const ReplaceableArgSet *, 4> TransformedSets;
    NewArgs.reserve(Call->arg_size() - ToBeRemoved.size() + Sets.size());

    for (auto [OldArgIdx, OldArg] : enumerate(Call->args())) {
        if (!ToBeRemoved.contains(OldArgIdx)) {
            // Copy unchanged argument
            NewArgs.push_back(OldArg.get());
        } else {
            // Consolidated argument
            assert(OldIdxToSet.contains(OldArgIdx));
            auto &Set = *OldIdxToSet.at(OldArgIdx);
            // Only run once per consolidated argument set
            if (TransformedSets.contains(&Set))
                continue;
            TransformedSets.insert(&Set);

            AllocaInst *AllocA = new AllocaInst(Set.Alloc->getAllocatedType(),
                                                NewF->getAddressSpace(),
                                                Twine("InsertedAllocA"), Call);
            bool Found = false;
            for (const auto &[C, CallInfo] : Set.Calls) {
                if (C != Call)
                    continue;
                if (CallInfo.StmntBlock != nullptr) {
                    if (StmntBlock != nullptr &&
                        StmntBlock != CallInfo.StmntBlock) {
                        ErrorReporter::addError(SOURCE_LOC,
                                                "Transformation failed, "
                                                "multiple stmnt blocks in call",
                                                *Call);
                    }
                    StmntBlock = CallInfo.StmntBlock;
                }
                if (CallInfo.GhostAssignBlock != nullptr) {
                    if (GhostAssignBlock != nullptr &&
                        GhostAssignBlock != CallInfo.GhostAssignBlock) {
                        ErrorReporter::addError(
                            SOURCE_LOC,
                            "Transformation failed, multiple "
                            "ghost assign blocks in call",
                            *Call);
                    }
                    GhostAssignBlock = CallInfo.GhostAssignBlock;
                }
                for (const auto &[_Offset, Source] : CallInfo.Fields) {
                    const auto &[Idx, Field] = Source;
                    auto *GEP = GetElementPtrInst::CreateInBounds(
                        Set.Alloc->getAllocatedType(), AllocA,
                        ArrayRef(
                            new Value *[]{ConstantInt::get(NewF->getContext(),
                                                           APInt(32, 0)),
                                          ConstantInt::get(NewF->getContext(),
                                                           APInt(32, Idx))},
                            2),
                        Twine("InsertedCallerGEP"), Call);
                    if (Field == NULL) {
                        break;
                    }
                    if (auto *I = dyn_cast<Instruction>(Field)) {
                        if (I->getParent() == NULL) {
                            for (Use &U : I->operands()) {
                                if (auto *I2 = dyn_cast<Instruction>(U.get())) {
                                    if (I2->getParent() == NULL)
                                        I2->insertBefore(Call);
                                }
                            }
                            I->insertBefore(Call);
                        }
                    }
                    new StoreInst(Field, GEP, Call);
                }

                Found = true;
                break;
            }
            if (!Found) {
                ErrorReporter::addError(SOURCE_LOC,
                                        "Transformation failed, call set was "
                                        "missing a call to the function");
            }
            NewArgs.push_back(AllocA);
        }
    }

    TransformedSets.clear();
    AttributeList PAL = Call->getAttributes();
    if (!PAL.isEmpty()) {
        SmallVector<AttributeSet, 8> ArgAttrs;
        for (unsigned OldArgIdx = 0; OldArgIdx < Call->arg_size();
             ++OldArgIdx) {
            if (!ToBeRemoved.contains(OldArgIdx)) {
                ArgAttrs.push_back(PAL.getParamAttrs(OldArgIdx));
            } else {
                // Add empty args for consolidated arg-set
                // (Only Once per set)
                auto *Set = OldIdxToSet.at(OldArgIdx);
                if (TransformedSets.contains(Set))
                    continue;
                TransformedSets.insert(Set);
                ArgAttrs.push_back(AttributeSet());
            }
        }
        PAL = AttributeList::get(OldF.getContext(), PAL.getFnAttrs(),
                                 PAL.getRetAttrs(), ArgAttrs);
    }

    SmallVector<OperandBundleDef, 1> OpBundles;
    Call->getOperandBundlesAsDefs(OpBundles);

    CallInst *newCall =
        CallInst::Create(NewF, ArrayRef(NewArgs), OpBundles, "", Call);
    newCall->setTailCallKind(Call->getTailCallKind());
    newCall->setCallingConv(Call->getCallingConv());
    newCall->setAttributes(PAL);
    newCall->copyMetadata(*Call, {LLVMContext::MD_prof, LLVMContext::MD_dbg});
    if (StmntBlock != nullptr)
        newCall->setMetadata(constants::PALLAS_SPEC_STMNT_BLOCK, StmntBlock);
    if (GhostAssignBlock != nullptr)
        newCall->setMetadata(constants::PALLAS_GHOST_ASSIGN_BLOCK,
                             GhostAssignBlock);

    Call->replaceAllUsesWith(newCall);
    newCall->takeName(Call);
    // Copied from RecursivelyDeleteTrivallyDeadInstructions (because call is
    // not trivially dead)
    for (Use &OpU : Call->operands()) {
        Value *OpV = OpU.get();
        OpU.set(nullptr);

        if (!OpV->use_empty())
            continue;

        RecursivelyDeleteTriviallyDeadInstructions(OpV);
    }

    for (const auto &Set : Sets) {
        for (const auto &[C, CallInfo] : Set->Calls) {
            if (C != Call)
                continue;
            if (CallInfo.Intermediary != nullptr) {
                SmallSet<Value *, 8> Visited;
                removeRecursively(CallInfo.Intermediary, Visited);
            }
        }
    }
    Call->eraseFromParent();
}

bool StructConsolidatorPass::gatherWrites(
    const Function &F, const DataLayout &L, uint64_t Size, const Value &V,
    APInt Offset, WriteVec &Writes,
    SmallVectorImpl<Instruction *> &LaterWrites) {
    for (const Use &U : V.uses()) {
        User *I = U.getUser();
        if (auto *GEP = dyn_cast<GetElementPtrInst>(I)) {
            // Check if we are indeed ofsetting *from* "value" as a pointer
            if (U.getOperandNo() != 0)
                return false;
            APInt NewOffset = Offset;
            if (!GEP->accumulateConstantOffset(L, NewOffset))
                return false;
            if (!gatherWrites(F, L, Size, *GEP, NewOffset, Writes, LaterWrites))
                return false;
        } else if (auto *Store = dyn_cast<StoreInst>(I)) {
            TypeSize size =
                L.getTypeSizeInBits(Store->getValueOperand()->getType());
            // We don't support type sizes parameterized with vscale
            if (size.isScalable())
                return false;

            Value *InnerV = Store->getValueOperand();
            while (auto *Cast = dyn_cast<CastInst>(InnerV)) {
                // Loop until V become the argument
                InnerV = Cast->getOperand(0);
            }
            // We only allow store's originating from arguments
            if (!isa<Argument>(InnerV)) {
                LaterWrites.push_back(Store);
                continue;
            }
            // We only allow byte-aligned stores
            if (size.getFixedValue() % 8 != 0) {
                LaterWrites.push_back(Store);
                continue;
            }
            Write W = {Offset.getLimitedValue(), size.getFixedValue() / 8,
                       InnerV, Store};
            Writes.push_back(W);
        } else if (auto *Call = dyn_cast<CallInst>(I)) {
            // Check for memcpy
            Function *IF = Call->getCalledFunction();
            // If we are calling a spec lib function then it will not have
            // side-effects
            if (IF->hasMetadata(constants::PALLAS_SPEC_LIB_MARKER))
                continue;

            // Check if we are indeed writing to our value (otherwise we are the
            // destination, skip)
            if (U.getOperandNo() != 0)
                continue;

            // We only support memcpys of the whole struct
            if (IF->getIntrinsicID() != Intrinsic::memcpy || !Offset.isZero()) {
                LaterWrites.push_back(Store);
                return false;
            }

            Value *Src = Call->getArgOperand(1);
            // Expecting src is an alloca of a struct with the same size as our
            // struct
            if (!isa<AllocaInst>(Src))
                return false;
            AllocaInst *SrcI = cast<AllocaInst>(Src);
            auto SrcSize = SrcI->getAllocationSize(L);
            if (!SrcSize.has_value() || *SrcSize < Size)
                return false;

            Value *Length = Call->getArgOperand(2);
            // Expecting len is an integer equal to the size of our struct
            if (!isa<ConstantInt>(Length))
                return false;
            if (!cast<ConstantInt>(Length)->equalsInt(Size))
                return false;

            Write W = {Offset.getLimitedValue(), Size, SrcI, Call};
            Writes.push_back(W);
        } else if (isa<LoadInst>(I)) {
            // Don't traverse further when we find a load
        } else if (!(utils::isPallasExprWrapper(F) ||
                     utils::isPallasGhostWrapper(F))) {
            LaterWrites.push_back(Store);
        }
    }

    return true;
}

StructConsolidatorPass::ReplaceableVec
StructConsolidatorPass::findReplaceableSets(Function &F, const DataLayout &L,
                                            const DominatorTree &DT) {
    const auto CompareArgs = [&](const ArgInfo &X, const ArgInfo &Y) {
        return X.Arg < Y.Arg;
    };
    const auto EqualArgs = [&](const ArgInfo &X, const ArgInfo &Y) {
        return X.Arg == Y.Arg;
    };
    const unsigned int AS = L.getAllocaAddrSpace();

    ReplaceableVec Sets;
    SmallSet<const AllocaInst *, 8> Intermediaries;
    AllocaMap AllocAs;
    // Find alloca's
    // Find indexes into the allocated object
    // Find writes to the allocated object or its indices

    // Create set of alloca's and writes to offsets within the struct
    // Check if there are no duplicate writes
    // Check if all writes originate from the input
    for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
            auto *AllocA = dyn_cast<AllocaInst>(&I);
            if (!AllocA)
                continue;
            // We only want to find alloca's allocating space for a single
            // struct
            Type *type = AllocA->getAllocatedType();
            if (AllocA->isArrayAllocation() || !isa<StructType>(type))
                continue;
            auto *ST = cast<StructType>(type);
            TypeSize Size = L.getTypeSizeInBits(ST);
            // We don't support type sizes parameterized with vscale
            if (Size.isScalable())
                continue;
            // We only allow byte-aligned stores
            if (Size.getFixedValue() % 8 != 0)
                continue;

            SmallVector<Write> Writes;
            SmallVector<Instruction *> LaterWrites;
            if (!gatherWrites(F, L, Size.getFixedValue() / 8, *AllocA,
                              APInt(L.getPointerSizeInBits(AS), 0, false),
                              Writes, LaterWrites))
                continue;

            bool Valid = true;

            for (const Instruction *I : LaterWrites) {
                for (const Write &W : Writes) {
                    if (!DT.dominates(W.WriteI, I)) {
                        Valid = false;
                        break;
                    }
                }
            }

            if (!Valid)
                continue;

            const auto *SL = L.getStructLayout(ST);

            IntervalSet Intervals;
            for (const Write &W : Writes) {
                Intervals.add(W.Offset, W.Offset + W.Size);
            }

            for (size_t Idx = 0, E = ST->getNumElements(); Idx < E; ++Idx) {
                if (SL->getElementOffset(Idx).isScalable()) {
                    Valid = false;
                    break;
                }
                uint64_t Offset = SL->getElementOffset(Idx).getFixedValue();
                // How does this work if getTypeStoreSize is not a multiple of
                // 8?
                if (!Intervals.contains(
                        Offset,
                        Offset + L.getTypeStoreSize(ST->getElementType(Idx)))) {
                    Valid = false;
                    break;
                }
            }
            if (Valid)
                AllocAs.insert({AllocA, Writes});
        }
    }
    for (std::pair<AllocaInst *, WriteVec> AllocA : AllocAs) {
        ReplaceableArgSet Set;
        Set.Alloc = AllocA.first;
        Set.Intermediary = NULL;
        Set.Valid = true;
        for (Write &W : AllocA.second) {
            assert(W.Src != NULL);
            if (auto *Intermediary = dyn_cast<AllocaInst>(W.Src)) {
                if (const auto &It = AllocAs.find(Intermediary);
                    It != AllocAs.end() && Set.Intermediary == NULL) {
                    Set.Intermediary = Intermediary;
                    continue;
                } else {
                    ErrorReporter::addWarning(SOURCE_LOC,
                                              "Not adding because invalid");
                    Set.Valid = false;
                    break;
                }
            } else {
                assert(isa<Argument>(W.Src) &&
                       "Expected the write src to be an alloca or function "
                       "argument");
                Set.Arguments.push_back(
                    {cast<Argument>(W.Src), W.Offset, W.Size});
            }
        }

        // Only consolidate neighbouring arguments to keep the ordering intact.
        if (!checkArgsNeighboring(Set)) {
            ErrorReporter::addWarning(
                SOURCE_LOC, "Not adding because arguments are not neighboring");
            Set.Valid = false;
        }

        if (Set.Valid) {
            if (Set.Intermediary != NULL)
                Intermediaries.insert(Set.Intermediary);
            llvm::sort(Set.Arguments.begin(), Set.Arguments.end(), CompareArgs);

            assert(std::unique(Set.Arguments.begin(), Set.Arguments.end(),
                               EqualArgs) == Set.Arguments.end());
            Sets.push_back(Set);
        }
    }
    for (auto &Set : Sets) {
        if (!Set.Valid || Intermediaries.contains(Set.Alloc))
            continue;

        if (Set.Intermediary) {
            ReplaceableArgSet *OtherSet = nullptr;

            for (ReplaceableArgSet *OS = Sets.begin(), *E = Sets.end(); OS != E;
                 ++OS) {
                if (OS->Alloc == Set.Intermediary) {
                    OtherSet = OS;
                    break;
                }
            }

            assert(OtherSet != nullptr);

            if (OtherSet->Valid) {
                for (ArgInfo A : OtherSet->Arguments) {
                    Set.Arguments.push_back(A);
                }
                llvm::sort(Set.Arguments.begin(), Set.Arguments.end(),
                           CompareArgs);

                assert(std::unique(Set.Arguments.begin(), Set.Arguments.end(),
                                   EqualArgs) == Set.Arguments.end());
                OtherSet->Valid = false;
            }
        }

        gatherUseData(F, L, Set);
    }

    // Mutating loop
    for (auto It = Sets.begin(); It != Sets.end();) {
        if (!It->Valid || (!It->Intermediary && It->Arguments.empty()) ||
            Intermediaries.contains(It->Alloc)) {
            It = Sets.erase(It);
        } else {
            ++It;
        }
    }

    return Sets;
}

bool StructConsolidatorPass::checkArgsNeighboring(
    const ReplaceableArgSet &ArgSet) {
    llvm::SmallVector<unsigned, 4> argIndices;
    for (auto &a : ArgSet.Arguments)
        argIndices.push_back(a.Arg->getArgNo());
    llvm::sort(argIndices);
    for (int i = 0; i < argIndices.size() - 1; ++i)
        if (argIndices[i + 1] != argIndices[i] + 1)
            return false;
    return true;
}

void StructConsolidatorPass::replaceWrapperCallInContract(ValueAsMetadata *OldF,
                                                          ValueAsMetadata *NewF,
                                                          MDNode *Contract) {
    // Check all clauses:
    for (unsigned CIdx = 5; CIdx < Contract->getNumOperands(); ++CIdx) {
        auto *Clause = cast<MDNode>(Contract->getOperand(CIdx).get());
        if (Clause->getNumOperands() >= 5 && Clause->getOperand(2) == OldF) {
            Clause->replaceOperandWith(2, NewF);
        }
    }
}

void StructConsolidatorPass::replaceWrapperCallInLoopInv(ValueAsMetadata *OldF,
                                                         ValueAsMetadata *NewF,
                                                         MDNode *LoopInv) {
    // Check all clauses:
    for (unsigned CIdx = 2; CIdx < LoopInv->getNumOperands(); ++CIdx) {
        auto *Clause = cast<MDNode>(LoopInv->getOperand(CIdx).get());
        if (Clause->getNumOperands() >= 4 && Clause->getOperand(1) == OldF) {
            Clause->replaceOperandWith(1, NewF);
        }
    }
}

void StructConsolidatorPass::replaceWrapperCallInSpecBlock(
    ValueAsMetadata *OldF, ValueAsMetadata *NewF, MDNode *Block) {
    for (unsigned Idx = 1; Idx < Block->getNumOperands(); ++Idx) {
        auto *SElem = cast<MDNode>(Block->getOperand(Idx).get());
        if (SElem->getNumOperands() >= 5 && SElem->getOperand(2) == OldF) {
            SElem->replaceOperandWith(2, NewF);
        }
    }
}

const Function &
StructConsolidatorPass::updateFunction(Function &F,
                                       const ReplaceableVec &Sets) {
    // Based on DeadArgumentEliminationPass::removeDeadStuffFromFunction
    assert(!F.isVarArg());

    DenseMap<const Argument *, const ReplaceableArgSet *> ArgMapping;
    for (const auto &set : Sets) {
        for (const ArgInfo &A : set.Arguments) {
            ArgMapping[A.Arg] = &set;
        }
    }

    // Build new FunctionType:
    FunctionType *FTy = F.getFunctionType();
    std::vector<Type *> Params;
    SmallVector<AttributeSet> ArgAttrVec;
    const AttributeList &PAL = F.getAttributes();

    // Build args while maintaining order:
    size_t NewIdx = 0;
    DenseMap<size_t, size_t> OldToNewIdx;
    SmallSet<const ReplaceableArgSet *, 4> AddedSets;
    for (auto [OldArgIdx, OldArg] : llvm::enumerate(F.args())) {
        if (!ArgMapping.contains(&OldArg)) {
            // 1) Copy argument
            Params.push_back(OldArg.getType());
            ArgAttrVec.push_back(PAL.getParamAttrs(OldArgIdx));
        } else {
            // 2) Consolidate
            auto *ArgSet = ArgMapping.at(&OldArg);
            // Only execute for first arg of the set
            if (AddedSets.contains(ArgSet))
                continue;
            AddedSets.insert(ArgSet);
            // Add set to args
            Params.push_back(
                PointerType::get(F.getContext(), F.getAddressSpace()));
            AttrBuilder B(F.getContext());
            // TODO: This should be byref for wrapper functions (I don't think
            // it matters though)
            B.addByValAttr(ArgSet->Alloc->getAllocatedType());
            B.addAttribute(Attribute::NoUndef);
            ArgAttrVec.push_back(AttributeSet::get(F.getContext(), B));
        }
        // Maintain mapping of old to new argument indices
        // Only for the first argument of a set the index is added
        OldToNewIdx[OldArgIdx] = NewIdx;
        ++NewIdx;
    }

    // AllocSize attribute may refer to removed argument
    AttributeSet FnAttrs =
        PAL.getFnAttrs().removeAttribute(F.getContext(), Attribute::AllocSize);

    assert(ArgAttrVec.size() == Params.size());

    // TODO: Perhaps we also want to detect and transform cases where a small
    // struct is returned
    AttributeList NewPAL = AttributeList::get(F.getContext(), FnAttrs,
                                              PAL.getRetAttrs(), ArgAttrVec);

    FunctionType *NFTy = FunctionType::get(FTy->getReturnType(), Params, false);

    assert(NFTy != FTy);

    // Build new Function
    Function *NF = Function::Create(NFTy, F.getLinkage(), F.getAddressSpace());
    NF->copyAttributesFrom(&F);
    NF->setComdat(F.getComdat());
    NF->setAttributes(NewPAL);
    F.getParent()->getFunctionList().insert(F.getIterator(), NF);
    NF->takeName(&F);
    // NF->IsNewDbgInfoFormat = F->IsNewDbgInfoFormat;

    for (User *U : make_early_inc_range(F.users())) {
        if (auto *C = dyn_cast<CallInst>(U)) {
            replaceFunctionUse(C, F, NF, ArgMapping);
        }
    }

    NF->splice(NF->begin(), &F);

    // Replace uses of the old function's arguments
    for (auto [ArgIdx, Arg] : llvm::enumerate(F.args())) {
        if (!ArgMapping.contains(&Arg)) {
            // Replace use of unchanged argument
            assert(OldToNewIdx.contains(ArgIdx));
            Arg.replaceAllUsesWith(NF->getArg(OldToNewIdx.at(ArgIdx)));
        } else {
            // Replace use alloca belonging to consolidated argument
            // Only execute for first arg of the set
            if (!OldToNewIdx.contains(ArgIdx))
                continue;
            auto *Set = ArgMapping.at(&Arg);
            Set->Alloc->replaceAllUsesWith(NF->getArg(OldToNewIdx.at(ArgIdx)));
            Set->Alloc->eraseFromParent();
            if (Set->Intermediary) {
                SmallSet<Value *, 8> Visited;
                removeRecursively(Set->Intermediary, Visited);
            }
        }
    }

    // Copy Metadata & check if wrapper function
    const unsigned WrapperID =
        F.getContext().getMDKindID(constants::PALLAS_WRAPPER_FUNC);
    const unsigned GhostWrapperID =
        F.getContext().getMDKindID(constants::PALLAS_GHOST_WRAPPER_FUNC);
    bool IsWrapper = false;
    SmallVector<std::pair<unsigned, MDNode *>, 1> MDs;
    F.getAllMetadata(MDs);
    for (auto [KindID, Node] : MDs) {
        IsWrapper |= KindID == WrapperID;
        IsWrapper |= KindID == GhostWrapperID;
        NF->addMetadata(KindID, *Node);
    }

    if (NFTy != FTy && NF->getSubprogram()) {
        DISubprogram *SP = NF->getSubprogram();
        auto Temp = SP->getType()->cloneWithCC(llvm::dwarf::DW_CC_nocall);
        SP->replaceType(MDNode::replaceWithPermanent(std::move(Temp)));
    }

    if (IsWrapper) {
        // Look for a Pallas MDNode representing a call to the wrapper function
        // TODO: This should maybe be cached? Or maybe we could store a
        // reference to all of these in the !pallas.exprWrapper node?
        // SmallVector<MDNode *> Metas;
        auto *OldFMD = ValueAsMetadata::get(&F);
        auto *NewFMD = ValueAsMetadata::get(NF);
        for (Function &OtherF : *F.getParent()) {
            if (auto MD = utils::getPallasContract(OtherF))
                replaceWrapperCallInContract(OldFMD, NewFMD, MD);
            for (auto I = inst_begin(F), E = inst_end(F); I != E; ++I) {
                if (auto MD = utils::getSpecStmntBlock(*I))
                    replaceWrapperCallInSpecBlock(OldFMD, NewFMD, MD);
                if (auto MD = irspec::getGhostAssignBlockMD(*I))
                    replaceWrapperCallInSpecBlock(OldFMD, NewFMD, MD);
                if (auto MD = irspec::getGivenBindingBlockMD(*I))
                    replaceWrapperCallInSpecBlock(OldFMD, NewFMD, MD);
                if (auto *LoopID = I->getMetadata(LLVMContext::MD_loop))
                    if (auto MD = utils::getPallasLoopContract(*LoopID))
                        replaceWrapperCallInLoopInv(OldFMD, NewFMD, MD);
            }
        }
    }

    // Remove arguments from the old function have been consolidated
    for (auto &Arg : F.args()) {
        if (ArgMapping.contains(&Arg)) {
            SmallSet<Value *, 8> Visited;
            removeRecursively(&Arg, Visited);
        }
    }

    // While we've handled the calls we also need to update constants and
    // metadata
    F.replaceAllUsesWith(NF);

    F.eraseFromParent();

    return *NF;
}

PreservedAnalyses StructConsolidatorPass::run(Module &M,
                                              ModuleAnalysisManager &MAM) {
    bool MadeChanges = false;
    const DataLayout &L = M.getDataLayout();

    FunctionAnalysisManager &FAM =
        MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

    DenseMap<Function *, ReplaceableVec> transformableFunctions;

    for (Function &F : M) {
        // Ensure that this a function which we can safely transform:
        // - It is not a declaration and cannot change externally
        // - It is not a varargs function
        // - It has no parameters with the InAllocA or Preallocated attributes
        // (these imply extra assumptions the caller is making regarding the
        // value this function returns and how we treat the passed in arguments)
        // These checks match those in the DeadArgumentEliminationPass
        if (!F.hasExactDefinition() || F.isVarArg() ||
            F.getAttributes().hasAttrSomewhere(Attribute::InAlloca) ||
            F.getAttributes().hasAttrSomewhere(Attribute::Preallocated) ||
            F.hasFnAttribute(Attribute::Naked)) {
            continue;
        }

        if (ReplaceableVec Sets = findReplaceableSets(
                F, L, FAM.getResult<DominatorTreeAnalysis>(F));
            !Sets.empty()) {
            transformableFunctions.insert({&F, Sets});
        }
    }

    for (auto const &[F, sets] : transformableFunctions) {
        updateFunction(*F, sets);
        MadeChanges = true;
    }

    if (MadeChanges) {
        return PreservedAnalyses::none();
    } else {
        return PreservedAnalyses::all();
    }
}
} // namespace pallas
