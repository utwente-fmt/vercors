#include "Passes/Module/ProtobufPrinter.h"
#include "Passes/Module/RootContainer.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Origin/OriginProvider.h"
#include "Transform/Transform.h"

namespace vcllvm {
    const std::string SOURCE_LOC = "Passes::Module::ProtobufPrinter";

    using namespace llvm;

    PreservedAnalyses ProtobufPrinter::run(Module &M, ModuleAnalysisManager &MAM) {
        if (ErrorReporter::hasErrors()) {
            llvm::errs() << "[VCLLVM] Conversion failed with " << ErrorReporter::getErrorCount() << " errors\n";
            return PreservedAnalyses::all();
        }
        auto pProgram = MAM.getResult<RootContainer>(M).program;
        pProgram->CheckInitialized();
        std::cout << pProgram->SerializeAsString();
        return PreservedAnalyses::all();
    }

}
