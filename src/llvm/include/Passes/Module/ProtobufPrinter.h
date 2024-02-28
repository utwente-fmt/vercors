#ifndef PALLAS_PROTOBUFPRINTER_H
#define PALLAS_PROTOBUFPRINTER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>

namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class ProtobufPrinter : public AnalysisInfoMixin<ProtobufPrinter> {
  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
};
} // namespace pallas
#endif // PALLAS_PROTOBUFPRINTER_H
