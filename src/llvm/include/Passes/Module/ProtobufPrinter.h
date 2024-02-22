#ifndef VCLLVM_PROTOBUFPRINTER_H
#define VCLLVM_PROTOBUFPRINTER_H

#include <llvm/IR/PassManager.h>
#include "vct/col/ast/col.pb.h"

namespace vcllvm {
    using namespace llvm;
    namespace col = vct::col::ast;

    class ProtobufPrinter : public AnalysisInfoMixin<ProtobufPrinter> {
    public:
        PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
    };
}
#endif // VCLLVM_PROTOBUFPRINTER_H
