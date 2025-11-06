#include "Passes/Module/StructConsolidator.h"
#include "Util/Exceptions.h"
#include <algorithm>
#include <llvm-17/llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Operator.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/Casting.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Transforms/Utils/Local.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::StructConsolidator";

using namespace llvm;

struct Interval {
    uint64_t start;
    uint64_t end;
};

struct IntervalSet {
    SmallVector<Interval> intervals;

    void add(uint64_t start, uint64_t end) {
        assert(start < end);
        if (intervals.empty()) {
            intervals.push_back({start, end});
        } else {
            for (size_t i = 0, size = intervals.size(); i < size; ++i) {
                if (intervals[i].start > start) {
                    if (intervals[i].start <= end) {
                        intervals[i].start = start;
                        intervals[i].end = std::max(intervals[i].end, end);
                    } else {
                        intervals.insert(intervals.begin() + i, {start, end});
                    }
                } else {
                    if (intervals[i].end <= start) {
                        intervals[i].end = std::max(intervals[i].end, end);
                    } else if (i == end - 1) {
                        intervals.push_back({start, end});
                    }
                }
            }
        }
    }

    bool contains(uint64_t start, uint64_t end) {
        for (const Interval &i : intervals) {
            if (i.start < start) {
                if (end <= i.end) {
                    return true;
                }
            } else {
                return i.start == start && end <= i.end;
            } 
        }
        return false;
    }
};

// WARNING: This can remove a lot of things, be very careful when calling this
void StructConsolidatorPass::removeRecursively(Value *V) {
    errs() << "Recursively removing `";
    V->print(errs());
    errs() << "`\n";
    while (!V->user_empty()){
        removeRecursively(V->user_back());
    }
    if (Instruction *I = dyn_cast<Instruction>(V)) {
        for (Use &U : I->operands()) {
            Value *OpV = U.get();
            U.set(nullptr);

            if (!OpV->use_empty()) continue;

            errs() << "Recursively deleting `";
            OpV->print(errs());
            errs() << "`\n";
            RecursivelyDeleteTriviallyDeadInstructions(OpV);
        }
        I->eraseFromParent();
    }
}

void StructConsolidatorPass::removeParentless(Value *V) {
    if (isa<Instruction>(V)) {
        if (cast<Instruction>(V)->getParent()) return;
        for (auto &O : cast<Instruction>(V)->operands()){
            removeParentless(O);
        }
        V->deleteValue();
    }
}

bool StructConsolidatorPass::digToField(Value *V, const DataLayout &L, const StructType &structType, FieldMap &fields, ArgInfo &A, APInt offsetIntoSource, uint64_t offsetIntoField, size_t pointerDepth) {
    // For now let's not consider deeper nesting
    if (pointerDepth > 1) return false;

    auto &[i, field] = fields[A.offset + offsetIntoField];
    Type *elementType = structType.getStructElementType(i);
    // We only want to find one source for each field
    if (field != NULL) return false;
    if (pointerDepth == 0 && V->getType() == elementType) {
        // We found a good source!
        field = V;
        return true;
    }

    if (isa<LoadInst>(V)) {
        LoadInst &load = *cast<LoadInst>(V);
        return digToField(load.getPointerOperand(), L, structType, fields, A, offsetIntoSource, offsetIntoField, pointerDepth + 1);
    }

    if (isa<GetElementPtrInst>(V)) {
        GetElementPtrInst &gep = *cast<GetElementPtrInst>(V);
        if (!gep.accumulateConstantOffset(L, offsetIntoSource)) return false;

        return digToField(gep.getPointerOperand(), L, structType, fields, A, offsetIntoSource, offsetIntoField, pointerDepth);
    }

    if (isa<AllocaInst>(V)) {
        const AllocaInst &allocA = *cast<AllocaInst>(V);
        assert(pointerDepth == 1);

        if (structType.getElementType(i) == allocA.getAllocatedType()) {
            // This is our source, we just need to load it
            // Byte-align is fine since we're never generating this code
            field = new LoadInst(elementType, V, Twine("insertedLoad"), false, Align());
            return true;
        }

        if (!isa<StructType>(allocA.getAllocatedType())) {
            // While this could technically be an intermediary there should be no need to generate it like that since you could have a direct Load instruction
            return false;
        }

        StructType *allocStructType = cast<StructType>(allocA.getAllocatedType());
        const StructLayout *structLayout = L.getStructLayout(allocStructType);

        // Decompose into available fields!
        // We have A.size bytes that we are reading from this allocation
        // We will get all fields starting from field[A.offset + offsetIntoField]
        int64_t remaining = A.size;
        while (remaining > 0) {
            auto &[i2, field2] = fields[A.offset + offsetIntoField];
            assert (field2 == NULL);
            int sourceIndex = structLayout->getElementContainingOffset(offsetIntoSource.getLimitedValue());

            // Somehow we've ended up misaligned somewhere
            if (offsetIntoSource != structLayout->getElementOffset(sourceIndex)) return false;
            
            if (structType.getStructElementType(i2) == allocStructType->getStructElementType(sourceIndex)) {
                // Found a match
                field2 = new LoadInst(elementType,
                     GetElementPtrInst::Create(allocStructType, V,
                        ArrayRef(new Value *[]{
                            ConstantInt::get(structType.getContext(), APInt(32, 0)),
                            ConstantInt::get(structType.getContext(), APInt(32, i2))
                        }, 2)),
                      Twine("insertedLoad"), false, Align());
                const TypeSize offset = L.getTypeAllocSize(structType.getStructElementType(i2));
                offsetIntoSource += offset;
                offsetIntoField += offset.getFixedValue();
                remaining -= offset.getFixedValue();
            } else {
                // This must be an intermediary struct
                // TODO: Find memcpy
                return false;
            }
        }
        assert(remaining == 0);

        return true;
    }

    return false;
}

void StructConsolidatorPass::gatherUseData(const Function &F, const DataLayout &L, ReplaceableArgSet &set) {
    StructType *structType = cast<StructType>(set.alloc->getAllocatedType());
    const StructLayout *structLayout = L.getStructLayout(structType);
    const ArrayRef<TypeSize> offsets = structLayout->getMemberOffsets(); 
    for (const Use &U : F.uses()) {
        FieldMap fields(offsets.size());
        for (size_t i = 0; i < offsets.size(); ++i) {
            fields.insert({offsets[i].getFixedValue(), {i, nullptr}});
        }
        
        if (!isa<CallInst>(U.getUser())) {
            // Copy of logic from Function::hasAddressTaken
            const User *FUU = U.getUser();
            if (isa<BitCastOperator, AddrSpaceCastOperator>(U) && FUU->hasOneUse() && !FUU->user_begin()->user_empty()) FUU = *FUU->user_begin();
            if (llvm::all_of(FUU->users(), [](const User *U) {
                    if (const auto *GV = dyn_cast<GlobalVariable>(U))
                        return GV->hasName() && (GV->getName() == "llvm.compiler.used" || GV->getName() == "llvm.used");
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
            set.valid = false;
            return;
        }

        CallInst *call = cast<CallInst>(U.getUser());
        if (call->getCalledFunction() != &F || call->hasOperandBundles()) {
            set.valid = false;
            return;
        }
        const Use *P = call->arg_begin();
        // Find operands for every arg in set.arguments
        for (const Argument *A = F.arg_begin(), *E = F.arg_end(); A != E; ++A, ++P) {
            bool found = false;
            for (ArgInfo &A2 : set.arguments) {
                if (A == A2.argument) {
                    if (!digToField(P->get(), L, *structType, fields, A2, APInt(L.getPointerSizeInBits(F.getAddressSpace()), 0, false), 0, 0)) {
                        set.valid = false;
                        for (auto &[_k, v] : fields) {
                            auto &[_i, field] = v;
                            if (field != NULL && isa<Instruction>(field)) {
                                Instruction *I = cast<Instruction>(field);
                                removeParentless(I);
                            }
                        }
                        return;
                    }
                    found = true;
                    break;
                }
            }
            if (!found) {
                set.valid = false;
                return;
            }
        }
        // For each operand move up until we find a variable of type set.alloc->getAllocationType
        // If not found bail
        //  (alternatively we can look to see if we can find a value the matches the size / offset of the fields corresponding to the arg)
        //  (For example: %1 = alloca {i32, i32}; %2 = 10; %3 = load i64, ptr %1, align 4; call void f(%3, %2))
        // If found store origin (and intermediary), we'll allow multiple origins as long as they're all of the appropriate type
        set.calls.insert({call, fields});
    }
}


void StructConsolidatorPass::replaceFunctionUse(CallInst *call, const Function &oldF, Function *newF, const ReplaceableVec &sets) {
    DenseMap<size_t, std::pair<size_t, const ArgInfo &>> argToSetArg(sets.size());
    // SmallSet<const Argument *, 8> ToBeRemoved;
    // for (const ReplaceableArgSet &set : sets) {
    //     for (const ArgInfo &A : set.arguments) {
    //         ToBeRemoved.insert(A.argument);
    //     }
    // }
    for (size_t i = 0; i < sets.size(); ++i) {
        for (const ArgInfo &A : sets[i].arguments) {
            argToSetArg.insert({A.argument->getArgNo(), {i, A}});
        }
    }

    std::vector<Value *> newArgs;
    newArgs.reserve(call->arg_size() - argToSetArg.size() + sets.size());

    size_t ArgI = 0;
    for (const Use *A = call->arg_begin(), *E = call->arg_end(); A != E; ++A, ++ArgI) {
        if (!argToSetArg.contains(ArgI)) {
            newArgs.push_back(A->get());
        }
    }
    std::vector<AllocaInst *> allocAs;
    allocAs.reserve(sets.size());

    for (const ReplaceableArgSet &set : sets) {
        AllocaInst *allocA = new AllocaInst(set.alloc->getAllocatedType(), newF->getAddressSpace(), Twine("InsertedAllocA"), call);
        // Find appropriate call, loop over all the fields in the field map, GEP then STORE and done!
        bool found = false;
        for (const auto &[C, F] : set.calls) {
            if (C == call) {
                for (const auto &[_off, source] : F) {
                    const auto &[i, field] = source;
                    GetElementPtrInst *gep = GetElementPtrInst::CreateInBounds(set.alloc->getAllocatedType(), allocA, ArrayRef(new Value *[]{
                        ConstantInt::get(newF->getContext(), APInt(32, 0)),
                        ConstantInt::get(newF->getContext(), APInt(32, i))               
                    }, 2), Twine("InsertedCallerGEP"), call);
                    if (field == NULL) {
                        break;
                    }
                    if (isa<Instruction>(field)) {
                        Instruction *I = cast<Instruction>(field);
                        if (I->getParent() == NULL) {
                            for (Use &U : I->operands()) {
                                if (isa<Instruction>(U.get())) {
                                    Instruction *I2 = cast<Instruction>(U.get());
                                    if (I2->getParent() == NULL) {
                                        I2->insertBefore(call);
                                    }
                                }
                            }
                            I->insertBefore(call);
                        }
                     }
                    new StoreInst(field, gep, call);
                }

                found = true;
                break;
            }
        }
        if (!found) {
            ErrorReporter::addError(SOURCE_LOC, "Transformation failed, call set was missing a call to the function");
        }
        newArgs.push_back(allocA);
    }

    // Again most of this is from the DeadArgumentElimination pass
    AttributeList PAL = call->getAttributes();
    if (!PAL.isEmpty()) {
      SmallVector<AttributeSet, 8> ArgAttrs;
      for (unsigned ArgNo = 0; ArgNo < newArgs.size(); ++ArgNo)
        ArgAttrs.push_back(PAL.getParamAttrs(ArgNo));
      PAL = AttributeList::get(oldF.getContext(), PAL.getFnAttrs(),
                               PAL.getRetAttrs(), ArgAttrs);
    }


    SmallVector<OperandBundleDef, 1> OpBundles;
    call->getOperandBundlesAsDefs(OpBundles);

    CallInst *newCall = CallInst::Create(newF, ArrayRef(newArgs), OpBundles, "", call);
    newCall->setTailCallKind(call->getTailCallKind());
    newCall->setCallingConv(call->getCallingConv());
    newCall->setAttributes(PAL);
    newCall->copyMetadata(*call, {LLVMContext::MD_prof, LLVMContext::MD_dbg});

    call->replaceAllUsesWith(newCall);
    newCall->takeName(call);
    // Copied from RecursivelyDeleteTrivallyDeadInstructions (because call is not trivially dead)
    for (Use &OpU : call->operands()) {
        Value *OpV = OpU.get();
        OpU.set(nullptr);

        if (!OpV->use_empty()) continue;

        RecursivelyDeleteTriviallyDeadInstructions(OpV);
    }
    call->eraseFromParent();
}

bool StructConsolidatorPass::gatherWrites(const Function &F, const DataLayout& L, uint64_t typeSize, const Value &value, APInt currentOffset, WriteVec &writes) {
    for (const Use &U : value.uses()) {
        User *I = U.getUser();
        if (isa<GetElementPtrInst>(I)) {
            GetElementPtrInst *gep = cast<GetElementPtrInst>(I);
            // Check if we are indeed ofsetting *from* "value" as a pointer
            if (U.getOperandNo() != 0) return false;
            APInt newOffset = currentOffset;
            if (!gep->accumulateConstantOffset(L, newOffset))  return false;
            if (!gatherWrites(F, L, typeSize, *gep, newOffset, writes)) return false;
        } else if (isa<StoreInst>(I)) {
            StoreInst *store = cast<StoreInst>(I);
            TypeSize size = L.getTypeSizeInBits(store->getValueOperand()->getType());
            // We don't support type sizes parameterized with vscale
            if (size.isScalable()) return false;
            // We only allow store's originating from arguments
            if (!isa<Argument>(store->getValueOperand())) return false;
            // We only allow byte-aligned stores
            if (size.getFixedValue() % 8 != 0) return false;
            Write write = {currentOffset.getLimitedValue(), size.getFixedValue() / 8, store->getValueOperand()};
            writes.push_back(write);
        } else if (isa<CallInst>(I)) {
            // Check for memcpy
            CallInst *call = cast<CallInst>(I);
            Function *IF = call->getCalledFunction();
            if (IF->getIntrinsicID() != Intrinsic::memcpy) return false;

            // Check if we are indeed writing to our value (otherwise we are the destination, skip)
            if (U.getOperandNo() != 0) continue;

            // We only support memcpys of the whole struct
            if (!currentOffset.isZero()) return false;

            Value *src = call->getArgOperand(1);
            // Expecting src is an alloca of a struct with the same size as our struct
            if (!isa<AllocaInst>(src)) return false;
            AllocaInst *srcInstruction = cast<AllocaInst>(src);
            std::optional<TypeSize> srcSize = srcInstruction->getAllocationSize(L);
            if (!srcSize.has_value() || *srcSize < typeSize) return false;

            Value *len = call->getArgOperand(2);
            // Expecting len is an integer equal to the size of our struct
            if (!isa<ConstantInt>(len)) return false;
            if (!cast<ConstantInt>(len)->equalsInt(typeSize)) return false;

            Write write = { currentOffset.getLimitedValue(), typeSize, srcInstruction};
            writes.push_back(write);
        } else if (isa<LoadInst>(I)) {
            // Don't traverse further when we find a load
        } else {
            // What to do with other uses? Loads are fine but other stuff might not be allowable
            std::string message;
            {
                raw_string_ostream stream(message);
                stream << "Not considering `";
                value.printAsOperand(stream , true, F.getParent());
                stream << "` valid due to: `";
                I->print(stream);
            }
            ErrorReporter::addError(SOURCE_LOC, message);
            return false;
        }
    }

    return true;
}

StructConsolidatorPass::ReplaceableVec StructConsolidatorPass::findReplaceableSets(Function &F, const DataLayout &L) {
    const unsigned int addressSpace = L.getAllocaAddrSpace();
    ReplaceableVec replaceableSets;
    DenseMap<const AllocaInst *, size_t> intermediaries;
    AllocaMap allocas;
    // Find alloca's
    // Find indexes into the allocated object
    // Find writes to the allocated object or its indices

    // Create set of alloca's and writes to offsets within the struct
    // Check if there are no duplicate writes
    // Check if all writes originate from the input
    for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
            AllocaInst *allocA = dyn_cast<AllocaInst>(&I);
            if (!allocA) continue;
            // We only want to find alloca's allocating space for a single struct
            Type *type = allocA->getAllocatedType();
            if (allocA->isArrayAllocation() || !isa<StructType>(type)) continue;
            StructType *structType = cast<StructType>(type);
            TypeSize size = L.getTypeSizeInBits(structType);
            // We don't support type sizes parameterized with vscale
            if (size.isScalable()) continue;
            // We only allow byte-aligned stores
            if (size.getFixedValue() % 8 != 0) continue;
            
            SmallVector<Write> writes;
            if (gatherWrites(F, L, size.getFixedValue()/8, *allocA, APInt(L.getPointerSizeInBits(addressSpace), 0, false), writes)) {
                const StructLayout *structLayout = L.getStructLayout(structType);

                IntervalSet intervals;
                for (const Write &W : writes) {
                    intervals.add(W.offset, W.offset + W.size);
                }

                bool valid = true;
                for (size_t i = 0; i < structType->getNumElements(); ++i) {
                    if (structLayout->getElementOffset(i).isScalable()) {
                        valid = false;
                        break;
                    }
                    uint64_t offset = structLayout->getElementOffset(i).getFixedValue();
                    // How does this work if getTypeStoreSize is not a multiple of 8?
                    if (!intervals.contains(offset, offset + L.getTypeStoreSize( structType->getElementType(i)))) {
                        valid = false;
                        break;
                    }
                }
                if (valid) allocas.insert({allocA, writes});
            }
        }
    }
    for (std::pair<AllocaInst *, WriteVec> allocA : allocas) {
        ReplaceableArgSet set;
        set.alloc = allocA.first;
        set.intermediary = NULL;
        set.valid = true;
        for (Write &W : allocA.second) {
            assert(W.src != NULL);
            if (AllocaInst *intermediary = dyn_cast<AllocaInst>(W.src)) {
                if (const AllocaMap::const_iterator &search = allocas.find(intermediary); search != allocas.end() && set.intermediary == NULL) {
                    set.intermediary = intermediary;
                    continue;
                } else {
                    ErrorReporter::addWarning(SOURCE_LOC, "Not adding because invalid");
                    set.valid = false;
                    break;
                } 
            } else {
                assert(isa<Argument>(W.src) &&
                        "Expected the write src to be an alloca or function argument");
                set.arguments.push_back({cast<Argument>(W.src), W.offset, W.size});
            }
        }
        if (set.valid) {
            if (set.intermediary != NULL) intermediaries.insert({set.intermediary, replaceableSets.size()});
            std::sort(set.arguments.begin(), set.arguments.end(), [&](const ArgInfo &a, const ArgInfo &b) {
                return a.argument < b.argument;
            });

            assert(std::unique(set.arguments.begin(), set.arguments.end(), [&](const ArgInfo &a, const ArgInfo &b) {
                return a.argument == b.argument;
            }) == set.arguments.end());
            replaceableSets.push_back(set);
        }
    }
    for (ReplaceableArgSet &set : replaceableSets) {
        if (auto user = intermediaries.find(set.alloc); user != intermediaries.end()) {
            if (set.intermediary) {
                // We do not allow chaining intermediaries
                set.valid = false;
                replaceableSets[user->second].valid = false;
            } else {
                SmallVector<ArgInfo> &otherArgs = replaceableSets[user->second].arguments;
                for (ArgInfo A : set.arguments) {
                    otherArgs.push_back(A);
                }
                std::sort(otherArgs.begin(), otherArgs.end(), [&](const ArgInfo &a, const ArgInfo &b) {
                    return a.argument < b.argument;
                });

                assert(std::unique(otherArgs.begin(), otherArgs.end(), [&](const ArgInfo &a, const ArgInfo &b) {
                    return a.argument == b.argument;
                }) == otherArgs.end());
            }
        }

        gatherUseData(F, L, set);
    }

    // Mutating loop
    for(ReplaceableVec::const_iterator it = replaceableSets.begin(); it != replaceableSets.end();) {
        if (!it->valid || (!it->intermediary && it->arguments.empty()) || intermediaries.contains(it->alloc)) {
            it = replaceableSets.erase(it);
        } else {
            errs() << "Found replacable set `";
            it->alloc->print(errs());
            errs() << "`,\nintermediary: `";
            if (it->intermediary) {it->intermediary->print(errs());} else {errs() << "NULL";}
            errs() << "`,\nargs:\n";
            for (const ArgInfo &A : it->arguments) {
                errs() << "`";
                A.argument->print(errs());
                errs() << "` at " << A.offset << " for " << A.size << " bytes\n";
            }
            errs() << "`,\ncalls:\n";
            for (const auto &[call, fields] : it->calls) {
                errs() << "\t`";
                call->print(errs());
                errs() << "` sources:\n";
                for (const auto &[field, v] : fields) {
                    const auto &[i, src] = v;
                    errs() << "\t\t field: " << field << ", i: " << i << ", src: " << src << "\n";
                }
            }
            ++it;
        }
        
    }

    return replaceableSets;
}

const Function &StructConsolidatorPass::updateFunction(Function &F, const ReplaceableVec &sets) {
    // Based on DeadArgumentEliminationPass::removeDeadStuffFromFunction
    assert(!F.isVarArg());

    SmallSet<const Argument *, 8> ToBeRemoved;
    for (const ReplaceableArgSet &set : sets) {
        for (const ArgInfo &A : set.arguments) {
            ToBeRemoved.insert(A.argument);
        }
    }
    FunctionType *FTy = F.getFunctionType();
    std::vector<Type *> Params;
    Params.reserve(F.arg_size() - ToBeRemoved.size() + sets.size());
    SmallVector<AttributeSet> ArgAttrVec;
    const AttributeList &PAL = F.getAttributes();

    size_t ArgI = 0;
    for (const Argument *I = F.arg_begin(), *E = F.arg_end(); I != E; ++I, ++ArgI) {
        if (!ToBeRemoved.contains(I)) {
            Params.push_back(I->getType());
            ArgAttrVec.push_back(PAL.getParamAttrs(ArgI));
        }
    }

    const size_t newParamIdx = Params.size();
    for (const ReplaceableArgSet &set : sets) {
        Params.push_back(PointerType::get(F.getContext(), F.getAddressSpace()));
        AttrBuilder B(F.getContext());
        B.addByValAttr(set.alloc->getAllocatedType());
        B.addAttribute(Attribute::NoUndef);
        ArgAttrVec.push_back(AttributeSet::get(F.getContext(), B));
    }

    // AllocSize attribute may refer to removed argument
    AttributeSet FnAttrs = PAL.getFnAttrs().removeAttribute(F.getContext(), Attribute::AllocSize);

    assert(ArgAttrVec.size() == Params.size());

    // TODO: Perhaps we also want to detect and transform cases where a small struct is returned
    AttributeList NewPAL = AttributeList::get(F.getContext(), FnAttrs, PAL.getRetAttrs(), ArgAttrVec);

    FunctionType *NFTy = FunctionType::get(FTy->getReturnType(), Params, false);

    assert(NFTy != FTy);

    Function *NF = Function::Create(NFTy, F.getLinkage(), F.getAddressSpace());
    NF->copyAttributesFrom(&F);
    NF->setComdat(F.getComdat());
    NF->setAttributes(NewPAL);
    F.getParent()->getFunctionList().insert(F.getIterator(), NF);
    NF->takeName(&F);
    // NF->IsNewDbgInfoFormat = F->IsNewDbgInfoFormat;

    for (User *U : make_early_inc_range(F.users())) {
        if (CallInst *C = dyn_cast<CallInst>(U)) {
            replaceFunctionUse(C, F, NF, sets);
        }
    }

    NF->splice(NF->begin(), &F);

    ArgI = 0;
    for (Argument *I = F.arg_begin(), *E = F.arg_end(); I != E; ++I) {
        if (!ToBeRemoved.contains(I)) {
            I->replaceAllUsesWith(NF->getArg(ArgI));
            ++ArgI;
        }
    }

    for (; ArgI < Params.size(); ++ArgI) {
        const ReplaceableArgSet &set = sets[ArgI - newParamIdx];
        set.alloc->replaceAllUsesWith(NF->getArg(ArgI));
        set.alloc->eraseFromParent();
        if (set.intermediary) {
            RecursivelyDeleteTriviallyDeadInstructions(set.intermediary);
        }
    }
    for (Argument *I = F.arg_begin(), *E = F.arg_end(); I != E; ++I) {
        if (ToBeRemoved.contains(I)) {
            removeRecursively(I);
        }
    }

    SmallVector<std::pair<unsigned, MDNode *>, 1> MDs;
    F.getAllMetadata(MDs);
    for (auto [KindID, Node] : MDs)
        NF->addMetadata(KindID, *Node);

    if (NFTy != FTy && NF->getSubprogram()) {
        DISubprogram *SP = NF->getSubprogram();
        auto Temp = SP->getType()->cloneWithCC(llvm::dwarf::DW_CC_nocall);
        SP->replaceType(MDNode::replaceWithPermanent(std::move(Temp)));
    }

    // While we've handled the calls we also need to update constants and metadata
    F.replaceAllUsesWith(NF);

    F.eraseFromParent();

    return *NF;
}

PreservedAnalyses StructConsolidatorPass::run(Module &M,
                                              ModuleAnalysisManager &MAM) {
    bool madeChanges = false;
    const DataLayout &L = M.getDataLayout();

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

        if (ReplaceableVec sets = findReplaceableSets(F, L); !sets.empty()) {
            transformableFunctions.insert({&F, sets});
        }
    }

    for (auto const& [F, sets] : transformableFunctions) {
        Function &oldF = *F;
        const Function &newF = updateFunction(oldF, sets);
    }

    if (madeChanges) {
        return PreservedAnalyses::none();
    } else {
        return PreservedAnalyses::all();
    }
}
} // namespace pallas