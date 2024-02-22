#ifndef VCLLVM_ROOTCONTAINER_H
#define VCLLVM_ROOTCONTAINER_H

#include <llvm/IR/PassManager.h>
#include "vct/col/ast/col.pb.h"

namespace vcllvm {
    using namespace llvm;
    namespace col = vct::col::ast;

    class ProgramWrapper {
    public:
        std::shared_ptr<col::Program> program;
        bool invalidate(Module &M, const PreservedAnalyses &PA, ModuleAnalysisManager::Invalidator &);
    };

    class RootContainer : public AnalysisInfoMixin<RootContainer> {
        friend AnalysisInfoMixin<RootContainer>;
        static AnalysisKey Key;
    public:
        using Result = ProgramWrapper;

        Result run(Module &M, ModuleAnalysisManager &MAM);
    };
}
#endif // VCLLVM_ROOTCONTAINER_H
