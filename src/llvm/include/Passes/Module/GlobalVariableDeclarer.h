#ifndef PALLAS_GLOBALVARIABLEDECLARER_H
#define PALLAS_GLOBALVARIABLEDECLARER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>

namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class GlobalVariableDeclarerPass
    : public AnalysisInfoMixin<GlobalVariableDeclarerPass> {
  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
};
} // namespace pallas
#endif // PALLAS_GLOBALVARIABLEDECLARER_H
