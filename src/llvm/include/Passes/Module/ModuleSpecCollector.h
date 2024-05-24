#ifndef VCLLVM_MODULESPECCOLLECTOR_H
#define VCLLVM_MODULESPECCOLLECTOR_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>
/**
 * Pass that adds global specifications (i.e. not related to a loop or function) to the AST as unparsed strings. It's
 * VerCors job to parse the string into any global declaration as if it were in a spec comment.
 */
namespace vcllvm {
    using namespace llvm;
    namespace col = vct::col::ast;

    class ModuleSpecCollectorPass : public AnalysisInfoMixin<ModuleSpecCollectorPass> {
    private:
        std::shared_ptr <col::Program> pProgram;
    public:
        explicit ModuleSpecCollectorPass(std::shared_ptr <col::Program> pProgram);

        PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
    };
}
#endif //VCLLVM_MODULESPECCOLLECTOR_H
