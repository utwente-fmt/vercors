#include "Passes/Module/TransitiveAssumeAnalysis.h"

#include "IRSpec/PallasSpecDecoding.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::TransitiveAssumeAnalysis";
using namespace llvm;

/*
 *  TAAResult
 */

TAAResult::TAAResult(const llvm::SmallSet<llvm::Function *, 8> &Assumed)
    : Assumed(Assumed) {}

bool TAAResult::isAssumed(Function &F) { return Assumed.contains(&F); }

/*
 *  TransitiveAssumeAnalysis
 */

AnalysisKey TransitiveAssumeAnalysis::Key;

TAAResult TransitiveAssumeAnalysis::run(llvm::Module &M,
                                        llvm::ModuleAnalysisManager &MAM) {

    // Collect all functions that are annotated directly with 'transitively
    // assumed'
    SmallSet<Function *, 8> Assumed;
    for (auto &F : M.functions()) {
        auto *CMD = irspec::getContractMD(F);
        if (CMD == nullptr) {
            continue;
        }
        auto C = irspec::getContract(CMD);
        if (!C.has_value()) {
            continue;
        }
        if (C->assumed == irspec::ContractAssumeType::TRANSITIVE_ASSUME) {
            Assumed.insert(&F);
        }
    }

    // Make a list of all potential candidates. I.e. functions that are
    // - only called directly
    // - not an intrinsic
    // - do not already have a pallas-contract
    // and a map of all callers for each candidate
    SmallVector<Function *> Candidates;
    DenseMap<Function *, SmallSet<Function *, 8>> CalledBy;
    for (auto &F : M.functions()) {
        if (F.isIntrinsic() || irspec::hasPallasContract(F) ||
            irspec::hasExternalPallasContract(F)) {
            continue;
        }
        bool IndirectUse = false;
        llvm::SmallSet<Function *, 8> Callers;
        for (auto &U : F.uses()) {
            auto CB = dyn_cast<CallBase>(U.getUser());
            if (CB == nullptr || !CB->isCallee(&U)) {
                IndirectUse = true;
                break;
            }
            Callers.insert(CB->getFunction());
        }

        if (!IndirectUse && !Callers.empty()) {
            Candidates.push_back(&F);
            CalledBy[&F] = Callers;
        }
    }

    // Do a fixed-point iteration and add all functions to the set that are
    // only called from functions that are already in the Set.
    // TODO: This is simple, but ineficient. Perhaps we want to optimize this.
    bool Change = true;
    while (Change) {
        // In each iteration-round, we first add to AddedF which is then merged
        // with Assumed to make sure that the iteration-order is irrelevant.
        SmallSet<Function *, 8> AddedF;
        for (auto *F : Candidates) {
            if (Assumed.contains(F)) {
                continue;
            }
            // Check if all callers are in the Assumed-set
            bool AllCallersAssumed = true;
            for (auto *Caller : CalledBy.at(F)) {
                if (!Assumed.contains(Caller)) {
                    AllCallersAssumed = false;
                    break;
                }
            }
            if (AllCallersAssumed) {
                AddedF.insert(F);
            }
        }
        Change = !AddedF.empty();
        Assumed.insert(AddedF.begin(), AddedF.end());
    }

    return TAAResult(Assumed);
}

} // namespace pallas