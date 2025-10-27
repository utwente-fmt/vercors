#ifndef PALLAS_PROTOBUFPRINTER_H
#define PALLAS_PROTOBUFPRINTER_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
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
