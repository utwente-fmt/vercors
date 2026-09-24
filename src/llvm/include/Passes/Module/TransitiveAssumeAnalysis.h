#ifndef PALLAS_TRANSITIVE_ASSUME_ANALYSIS_H
#define PALLAS_TRANSITIVE_ASSUME_ANALYSIS_H

#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>

/**
 * Analysis pass that analyses the module to find all functions that are
 * affected by Pallas-contracts with the 'transitively assumed'-flag.
 *
 * This pass collects all functions which are marked as 'transitively assumed'
 * or which are only called from functions that are themselves considered
 * transitively assumed.
 *
 * TODO: This currently does not extend into recursive cycles.
 */
namespace pallas {

class TAAResult {
    friend class TransitiveAssumeAnalysis;

  private:
    llvm::SmallSet<llvm::Function *, 8> Assumed;

  public:
    explicit TAAResult(const llvm::SmallSet<llvm::Function *, 8> &Assumed);

    bool isAssumed(llvm::Function &F);
};

class TransitiveAssumeAnalysis
    : public llvm::AnalysisInfoMixin<TransitiveAssumeAnalysis> {
    friend llvm::AnalysisInfoMixin<TransitiveAssumeAnalysis>;
    static llvm::AnalysisKey Key;

  public:
    using Result = TAAResult;

    /**
     * Builds the set of functions that should be assumed to be correct based on
     * contracts with the 'transitively assumed' keyword.
     */
    Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};

} // namespace pallas
#endif // PALLAS_TRANSITIVE_ASSUME_ANALYSIS_H