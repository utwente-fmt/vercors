#ifndef PALLAS_MODULESPECCOLLECTOR_H
#define PALLAS_MODULESPECCOLLECTOR_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>
/**
 * Pass that adds global specifications (i.e. not related to a loop or function)
 * to the AST as unparsed strings. It's VerCors job to parse the string into any
 * global declaration as if it were in a spec comment.
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class ModuleSpecCollectorPass
    : public AnalysisInfoMixin<ModuleSpecCollectorPass> {
  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
};
} // namespace pallas
#endif // PALLAS_MODULESPECCOLLECTOR_H
