#include "Passes/Module/ProtobufPrinter.h"
#include "Passes/Module/RootContainer.h"
#include "Util/Exceptions.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::ProtobufPrinter";

using namespace llvm;

PreservedAnalyses ProtobufPrinter::run(Module &M, ModuleAnalysisManager &MAM) {
    if (ErrorReporter::hasErrors()) {
        llvm::errs() << "[ERROR] [pallas] Conversion failed with "
                     << ErrorReporter::getWarningCount() << " warnings and "
                     << ErrorReporter::getErrorCount() << " errors\n";
    } else {
        llvm::errs() << "[INFO] [pallas] Conversion succeeded with "
                     << ErrorReporter::getWarningCount() << "warnings\n";
    }
    auto pProgram = MAM.getResult<RootContainer>(M).program;
    if (pProgram->IsInitialized()) {
        std::cout << pProgram->SerializeAsString();
    } else {
        llvm::errs() << "[ERROR] [pallas] Internal error, invalid protobuf "
                        "construction\n";
        pProgram->CheckInitialized();
    }
    return PreservedAnalyses::all();
}

} // namespace pallas
