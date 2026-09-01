#include "Passes/Module/StructConsolidator.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"
#include <algorithm>
#include <llvm/ADT/ArrayRef.h>
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
#include <llvm/IR/IRBuilder.h>
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
#include <optional>
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
    size_t Depth, MDNode **StmntBlock) {
    // For now let's not consider deeper nesting
    if (Depth > 1)
        return Fail{};

    // Given- / Yields-bindings are only attached to call-instructions.
    // So we do not check for multiple annotations here.
    if (auto *I = dyn_cast<Instruction>(V)) {
        if (auto *Block = irspec::getStmntBlockMD(*I)) {
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
                          SourceOffset, FieldOffset, Depth + 1, StmntBlock);
    }

    if (auto *GEP = dyn_cast<GetElementPtrInst>(V)) {
        if (!GEP->accumulateConstantOffset(L, SourceOffset))
            return Fail{};

        return digToField(F, GEP->getPointerOperand(), L, ST, Fields, A,
                          SourceOffset, FieldOffset, Depth, StmntBlock);
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
    // vvv Check every use of the consoliated function
    for (const Use &U : F.uses()) {
        FieldMap Fields(Offsets.size());
        for (size_t Idx = 0, E = Offsets.size(); Idx < E; ++Idx) {
            Fields.insert({Offsets[Idx].getFixedValue(), {Idx, nullptr}});
        }

        // vvv Filter out functions that are used outside of a call
        if (!isa<CallInst>(U.getUser())) {
            // Copy of logic from Function::hasAddressTaken
            const User *FUU = U.getUser();
            if (isa<BitCastOperator, AddrSpaceCastOperator>(U) &&
                FUU->hasOneUse() && !FUU->user_begin()->user_empty())
                FUU = *FUU->user_begin();
            // vvv Allow uses in llvm.used that are not a call.
            if (llvm::all_of(FUU->users(), [](const User *U) {
                    if (const auto *GV = dyn_cast<GlobalVariable>(U))
                        return GV->hasName() &&
                               (GV->getName() == "llvm.compiler.used" ||
                                GV->getName() == "llvm.used");
                    return false;
                }))
                continue;

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
        const Use *P = Call->arg_begin();
        // Find operands for every arg in set.arguments
        for (const Argument *FA = F.arg_begin(), *E = F.arg_end(); FA != E;
             ++FA, ++P) {
            std::optional<DigToFieldResult> Result = std::nullopt;
            for (ArgInfo &SA : Set.Arguments) {
                if (FA != SA.Arg) {
                    continue;
                }
                Result = digToField(
                    F, P->get(), L, *ST, Fields, SA,
                    APInt(L.getPointerSizeInBits(F.getAddressSpace()), 0,
                          false),
                    0, 0, &StmntBlock);

                if (!std::holds_alternative<Fail>(*Result)) {
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

            if (!Result) {
                // Skip args that to not belong to a consolidated set.
                continue;
            }

            assert(Result.has_value());
            if (std::holds_alternative<Fail>(*Result)) {
                Set.Valid = false;
                return;
            } else if (auto FA = std::get_if<FoundAll>(&*Result)) {
                Intermediary = FA->Intermediary;
                break;
            } else {
                assert(std::holds_alternative<Found>(*Result));
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
        // TODO: If fields points to an argument of the function in which the 
        // call is made, this will fail if the functions are transformed in the 
        // wrong order! 
        Set.Calls.insert({Call, {Fields, Intermediary, StmntBlock}});
    }
}

void StructConsolidatorPass::replaceFunctionUse(CallInst *Call,
                                                const Function &OldF,
                                                Function *NewF,
                                                const ReplaceableVec &Sets) {
    SmallSet<size_t, 8> ToBeRemoved;
    for (const auto &Set : Sets) {
        for (const ArgInfo &A : Set.Arguments) {
            ToBeRemoved.insert(A.Arg->getArgNo());
        }
    }

    std::vector<Value *> NewArgs;
    NewArgs.reserve(Call->arg_size() - ToBeRemoved.size() + Sets.size());

    size_t ArgI = 0;
    for (const Use *A = Call->arg_begin(), *E = Call->arg_end(); A != E;
         ++A, ++ArgI) {
        if (!ToBeRemoved.contains(ArgI)) {
            NewArgs.push_back(A->get());
        }
    }
    std::vector<AllocaInst *> AllocAs;
    AllocAs.reserve(Sets.size());

    auto *StmntBlock = irspec::getStmntBlockMD(*Call);
    auto *GivenBinding = irspec::getGivenBindingBlockMD(*Call);
    auto *YieldsBinding = irspec::getYieldsBindingBlockMD(*Call);

    for (const auto &Set : Sets) {
        AllocaInst *AllocA = new AllocaInst(Set.Alloc->getAllocatedType(),
                                            NewF->getAddressSpace(),
                                            Twine("InsertedAllocA"), Call);
        CallSiteAllocas.insert(AllocA);
        bool Found = false;
        for (const auto &[C, CallInfo] : Set.Calls) {
            if (C != Call)
                continue;
            if (CallInfo.StmntBlock != nullptr) {
                if (StmntBlock != nullptr &&
                    StmntBlock != CallInfo.StmntBlock) {
                    ErrorReporter::addError(
                        SOURCE_LOC,
                        "Transformation failed, multiple stmnt blocks in call",
                        *Call);
                }
                StmntBlock = CallInfo.StmntBlock;
            }
            for (const auto &[_Offset, Source] : CallInfo.Fields) {
                const auto &[Idx, Field] = Source;
                auto *GEP = GetElementPtrInst::CreateInBounds(
                    Set.Alloc->getAllocatedType(), AllocA,
                    ArrayRef(new Value *[]{ConstantInt::get(NewF->getContext(),
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

    // Again most of this is from the DeadArgumentElimination pass
    AttributeList PAL = Call->getAttributes();
    SmallVector<AttributeSet, 8> ArgAttrs;
    // Copy attributes of unchanged arguments
    for (unsigned ArgIdx = 0, NoArgs = Call->arg_size(); ArgIdx < NoArgs;
         ++ArgIdx) {
        if (!ToBeRemoved.contains(ArgIdx)) {
            ArgAttrs.push_back(PAL.getParamAttrs(ArgIdx));
        }
    }
    // Add attriutes to consolidated arguments based on signature of new,
    // called function
    auto NewFAttributes = NewF->getAttributes();
    for (unsigned ArgIdx = ArgAttrs.size(), NoNewArgs = NewArgs.size();
         ArgIdx < NoNewArgs; ++ArgIdx) {
        ArgAttrs.push_back(NewFAttributes.getParamAttrs(ArgIdx));
    }

    auto NewPAL = AttributeList::get(OldF.getContext(), PAL.getFnAttrs(),
                                     PAL.getRetAttrs(), ArgAttrs);

    SmallVector<OperandBundleDef, 1> OpBundles;
    Call->getOperandBundlesAsDefs(OpBundles);

    CallInst *newCall =
        CallInst::Create(NewF, ArrayRef(NewArgs), OpBundles, "", Call);
    newCall->setTailCallKind(Call->getTailCallKind());
    newCall->setCallingConv(Call->getCallingConv());
    newCall->setAttributes(NewPAL);
    newCall->copyMetadata(*Call, {LLVMContext::MD_prof, LLVMContext::MD_dbg});
    if (StmntBlock != nullptr)
        newCall->setMetadata(constants::PALLAS_SPEC_STMNT_BLOCK, StmntBlock);
    if (GivenBinding != nullptr)
        newCall->setMetadata(constants::PALLAS_GIVEN_BINDING_BLOCK,
                             GivenBinding);
    if (YieldsBinding != nullptr)
        newCall->setMetadata(constants::PALLAS_YIELDS_BINDING_BLOCK,
                             YieldsBinding);

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
        for (const auto &[C, CallInfo] : Set.Calls) {
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
            if (irspec::isPallasSpecLib(*IF) != std::nullopt)
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
        } else if (!(F.hasMetadata(constants::PALLAS_WRAPPER_FUNC) ||
                     F.hasMetadata(constants::PALLAS_GHOST_WRAPPER_FUNC))) {
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

            // Skip Allocas that were inserted by this pass itself on the 
            // call-site of function. 
            if (CallSiteAllocas.contains(AllocA)) {
                continue;
            } 

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

            // Check that the initializing writes occur before any other writes
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

            // Check that the initializing writes cover the full struct
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

    // vvv Try to map the pair of alloca and initializing writes to arguments
    for (std::pair<AllocaInst *, WriteVec> AllocA : AllocAs) {
        ReplaceableArgSet Set;
        Set.Alloc = AllocA.first;
        Set.Intermediary = NULL;
        Set.Valid = true;

        for (Write &W : AllocA.second) {
            Set.Writes.push_back(W.WriteI);
        }

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
                auto *Arg = cast<Argument>(W.Src);
                // Check that if direct uses of the arg exist (i.e. uses that do
                // not belong to the writes that initialize the alloca) then the
                // arg directly maps to an element of the consolidated struct.
                if (!getDirectUsers(*Arg, Set.Writes).empty()) {
                    auto *ST =
                        dyn_cast<StructType>(AllocA.first->getAllocatedType());
                    assert(ST != nullptr);
                    auto elemIdx = getStructElemAtOffset(*ST, W.Offset, L);
                    if (!elemIdx ||
                        (ST->getElementType(*elemIdx) != Arg->getType())) {
                        AllocA.first->dump();
                        ST->getElementType(*elemIdx)->dump();
                        Arg->getType()->dump();
                        ErrorReporter::addWarning(
                            SOURCE_LOC,
                            "Not adding because invalid direct uses exist");
                        Set.Valid = false;
                        break;
                    }
                }

                Set.Arguments.push_back({Arg, W.Offset, W.Size});
            }
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

    // Propagate arguments accross intermediary & analyze call-sites
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
                for (auto *w : OtherSet->Writes) {
                    Set.Writes.push_back(w);
                }

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

void StructConsolidatorPass::replaceWrapperReferences(Function &WF,
                                                      Function &NWF) {
    // Check all functions for specifications that reference F:
    for (Function &OtherF : *WF.getParent()) {
        // Contract
        if (auto *contrMD = irspec::getContractMD(OtherF)) {
            auto contr = irspec::getContract(contrMD);
            for (auto &clause : contr->clauses) {
                replaceWrapper(clause, WF, NWF);
            }
        }

        // Check instructions
        for (auto I = inst_begin(OtherF), E = inst_end(OtherF); I != E; ++I) {
            // Stmnt-Block
            if (auto BMD = irspec::getStmntBlockMD(*I)) {
                auto Block = irspec::getSpecStatementBlock(BMD);
                for (auto &S : Block->statements) {
                    replaceWrapper(S, WF, NWF);
                }
            }
            // Loop Invariant
            if (auto *LID = I->getMetadata(LLVMContext::MD_loop)) {
                if (auto *LContrMD = irspec::getLoopContractMD(*LID)) {
                    auto LContr = irspec::getLoopContract(LContrMD);
                    for (auto &Inv : LContr->clauses) {
                        replaceWrapper(Inv, WF, NWF);
                    }
                }
            }
            // Given Binding
            if (auto BMD = irspec::getGivenBindingBlockMD(*I)) {
                auto Block = irspec::getGivenBindingBlock(BMD);
                for (auto &B : Block->bindings) {
                    replaceWrapper(B, WF, NWF);
                }
            }
        }
    }
}

void StructConsolidatorPass::replaceWrapper(irspec::WrappedSpecElement &S,
                                            llvm::Function &OWF,
                                            llvm::Function &NWF) {
    if (&S.getWrapper() != &OWF)
        return;
    S.getMD()->replaceOperandWith(2, llvm::ValueAsMetadata::get(&NWF));
}

SmallSet<llvm::User *, 8> StructConsolidatorPass::getDirectUsers(
    Argument &Arg, const SmallVector<Instruction *> &Writes) {

    SmallPtrSet<Instruction *, 8> writeSet;
    for (auto *w : Writes) {
        writeSet.insert(w);
    }

    llvm::SmallSet<User *, 8> users;
    for (auto *U : Arg.users()) {
        if (auto *I = dyn_cast<Instruction>(U)) {
            if (!writeSet.contains(I)) {
                users.insert(I);
            }
        }
    }

    return users;
}

std::optional<unsigned>
StructConsolidatorPass::getStructElemAtOffset(StructType &S, uint64_t Offset,
                                              const DataLayout &L) {
    // Get element that contains the offset
    auto SL = L.getStructLayout(&S);
    auto ElemIdx = SL->getElementContainingOffset(Offset);
    // Check that the offset is the start of the element
    if (SL->getElementOffset(ElemIdx) == Offset) {
        return std::make_optional(ElemIdx);
    }
    return std::nullopt;
}

const Function &
StructConsolidatorPass::updateFunction(Function &F, const ReplaceableVec &Sets,
                                       const DataLayout &L) {
    // Based on DeadArgumentEliminationPass::removeDeadStuffFromFunction
    assert(!F.isVarArg());

    SmallSet<const Argument *, 8> ToBeRemoved;
    for (const auto &set : Sets) {
        for (const ArgInfo &A : set.Arguments) {
            ToBeRemoved.insert(A.Arg);
        }
    }

    // vvv Construct new Function-type vvv
    FunctionType *FTy = F.getFunctionType();
    std::vector<Type *> Params;
    Params.reserve(F.arg_size() - ToBeRemoved.size() + Sets.size());
    SmallVector<AttributeSet> ArgAttrVec;
    const AttributeList &PAL = F.getAttributes();

    size_t ArgI = 0;
    for (const Argument *I = F.arg_begin(), *E = F.arg_end(); I != E;
         ++I, ++ArgI) {
        if (!ToBeRemoved.contains(I)) {
            Params.push_back(I->getType());
            ArgAttrVec.push_back(PAL.getParamAttrs(ArgI));
        }
    }

    const size_t NewIdx = Params.size();
    for (const auto &set : Sets) {
        Params.push_back(PointerType::get(F.getContext(), F.getAddressSpace()));
        AttrBuilder B(F.getContext());
        // TODO: This should be byref for wrapper functions (I don't think
        // it matters though)
        B.addByValAttr(set.Alloc->getAllocatedType());
        B.addAttribute(Attribute::NoUndef);
        ArgAttrVec.push_back(AttributeSet::get(F.getContext(), B));
    }

    // AllocSize attribute may refer to removed argument
    AttributeSet FnAttrs =
        PAL.getFnAttrs().removeAttribute(F.getContext(), Attribute::AllocSize);

    assert(ArgAttrVec.size() == Params.size());

    // TODO: Perhaps we also want to detect and transform cases where a
    // small struct is returned
    AttributeList NewPAL = AttributeList::get(F.getContext(), FnAttrs,
                                              PAL.getRetAttrs(), ArgAttrVec);

    FunctionType *NFTy = FunctionType::get(FTy->getReturnType(), Params, false);

    assert(NFTy != FTy);

    // vvv Setup function with new signature vvv
    Function *NF = Function::Create(NFTy, F.getLinkage(), F.getAddressSpace());
    NF->copyAttributesFrom(&F);
    NF->setComdat(F.getComdat());
    NF->setAttributes(NewPAL);
    F.getParent()->getFunctionList().insert(F.getIterator(), NF);
    NF->takeName(&F);
    // NF->IsNewDbgInfoFormat = F->IsNewDbgInfoFormat;

    for (User *U : make_early_inc_range(F.users())) {
        if (auto *C = dyn_cast<CallInst>(U)) {
            replaceFunctionUse(C, F, NF, Sets);
        }
    }

    NF->splice(NF->begin(), &F);

    ArgI = 0;
    for (Argument *I = F.arg_begin(), *E = F.arg_end(); I != E; ++I) {
        if (!ToBeRemoved.contains(I)) {
            I->replaceAllUsesWith(NF->getArg(ArgI));
            NF->getArg(ArgI)->takeName(I);
            ++ArgI;
        }
    }

    // vvv Point uses of the alloca to the new argument
    for (; ArgI < Params.size(); ++ArgI) {
        const auto &Set = Sets[ArgI - NewIdx];
        auto *NewArg = NF->getArg(ArgI);

        // vvv Fix direct uses of the old argument vvv
        for (auto Arg : Set.Arguments) {
            // Identify direct uses of the old argument that are not part of the
            // writes that initialize the alloca.
            auto Users = getDirectUsers(*Arg.Arg, Set.Writes);
            if (Users.empty()) {
                // Arg not used directly, so no intermediaries needed
                continue;
            }

            // vvv Insert intermediaries that replace the old argument

            // Get the index of the struct element to which the arg coresponds
            // The argument should correspond to an element of the struct type
            // due to the earlier check in findReplaceableSets
            auto ArgT = Arg.Arg->getType();
            assert(NewArg->hasByValAttr());
            assert(isa<StructType>(NewArg->getParamByValType()));
            auto StructT = cast<StructType>(NewArg->getParamByValType());
            auto ElemIdx = getStructElemAtOffset(*StructT, Arg.Offset, L);
            assert(ElemIdx);
            assert(ArgT == StructT->getElementType(*ElemIdx));

            // Insert intermediary to replace the old arg
            auto *insertPoint = &*NF->getEntryBlock().getFirstInsertionPt();
            IRBuilder Builder(insertPoint);
            auto IPtr = Builder.CreateStructGEP(StructT, NewArg, *ElemIdx);
            auto IntermArg = Builder.CreateLoad(
                ArgT, IPtr, Twine("old_") + Arg.Arg->getName());

            // vvv Point uses of the old arg to the new intermediary

            Arg.Arg->replaceUsesWithIf(IntermArg, [&Users](Use &U) {
                return Users.contains(U.getUser());
            });
        }

        Set.Alloc->replaceAllUsesWith(NewArg);
        Set.Alloc->eraseFromParent();
        if (Set.Intermediary) {
            SmallSet<Value *, 8> Visited;
            removeRecursively(Set.Intermediary, Visited);
        }
    }

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
        // Replace references to F in Pallas specifications
        replaceWrapperReferences(F, *NF);
    }

    for (Argument *I = F.arg_begin(), *E = F.arg_end(); I != E; ++I) {
        if (ToBeRemoved.contains(I)) {
            SmallSet<Value *, 8> Visited;
            removeRecursively(I, Visited);
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

    // TODO: If the consolidation introduces too many unneccessary de- and
    //       reconstructions of structs, we could consider to use the (reversed)
    //       scc_iterator to transform functions 'outside-in'
    SmallVector<Function *> OldFunctions;
    for (auto &F : M.functions()) {
        OldFunctions.push_back(&F);
    }

    for (Function *F : OldFunctions) {
        // Ensure that this a function which we can safely transform:
        // - It is not a declaration and cannot change externally
        // - It is not a varargs function
        // - It has no parameters with the InAllocA or Preallocated
        // attributes (these imply extra assumptions the caller is making
        // regarding the value this function returns and how we treat the
        // passed in arguments) These checks match those in the
        // DeadArgumentEliminationPass
        if (!F->hasExactDefinition() || F->isVarArg() ||
            F->getAttributes().hasAttrSomewhere(Attribute::InAlloca) ||
            F->getAttributes().hasAttrSomewhere(Attribute::Preallocated) ||
            F->hasFnAttribute(Attribute::Naked)) {
            continue;
        }

        // Gather sets of arguments that can be consolidated
        auto Sets = findReplaceableSets(
            *F, L, FAM.getResult<DominatorTreeAnalysis>(*F));
        if (Sets.empty()) {
            continue;
        }

        // Consolidate argument-sets
        updateFunction(*F, Sets, L);
        MadeChanges = true;
    }

    if (MadeChanges) {
        return PreservedAnalyses::none();
    } else {
        return PreservedAnalyses::all();
    }
}
} // namespace pallas
