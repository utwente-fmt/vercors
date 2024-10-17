#ifndef PALLAS_ROOTCONTAINER_H
#define PALLAS_ROOTCONTAINER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>

namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class ProgramWrapper {
  public:
    std::shared_ptr<col::Program> program;
    bool invalidate(Module &M, const PreservedAnalyses &PA,
                    ModuleAnalysisManager::Invalidator &);
};

class RootContainer : public AnalysisInfoMixin<RootContainer> {
    friend AnalysisInfoMixin<RootContainer>;
    static AnalysisKey Key;

  public:
    using Result = ProgramWrapper;

    Result run(Module &M, ModuleAnalysisManager &MAM);
};
} // namespace pallas
#endif // PALLAS_ROOTCONTAINER_H
