#ifndef PALLAS_PUREASSIGNER_H
#define PALLAS_PUREASSIGNER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>
/**
 * The PureAssignerPass checks if a LLVM function is pure (i.e. whether the
 * !VC.pure metadata node is set)
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class PureAssignerPass : public PassInfoMixin<PureAssignerPass> {
  public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
};
} // namespace pallas
#endif // PALLAS_PUREASSIGNER_H
