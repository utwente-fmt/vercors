#include "Passes/Module/TransitiveAssumeAnalysis.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::TransitiveAssumeAnalysis";
using namespace llvm;

/*
 *  TAAResult
 */

TAAResult::TAAResult() : Assumed() {}

void TAAResult::addAssumed(Function &F) { Assumed.insert(&F); }

bool TAAResult::isAssumed(Function &F) { return Assumed.contains(&F); }

/*
 *  TransitiveAssumeAnalysis
 */

AnalysisKey TransitiveAssumeAnalysis::Key;

TAAResult run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
    
    // Collect all functions that are directly annotated with 'transitively assumed'
    
    
    // TODO: Implement the actual analysis
    assert(false);
}

} // namespace pallas