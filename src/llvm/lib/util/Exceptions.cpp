#include "Util/Exceptions.h"
#include "Origin/ShortPositionDeriver.h"


#include <sstream>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Module.h>

namespace vcllvm {
    [[nodiscard]] const char *UnsupportedTypeException::what() const noexcept {
        return "Type not supported";
    }

    u_int32_t ErrorReporter::errorCount;

    void ErrorReporter::addError(const std::string &source, const std::string &message, const std::string &origin) {
        llvm::errs() << "[VCLLVM] [" << source << "] " << message << " @\n  " << origin << "\n\n";
        ErrorReporter::errorCount++;
    }

    void ErrorReporter::addError(const std::string &source,
                                 const std::string &message,
                                 llvm::Module &llvmModule) {
        addError(source, message, llvm2Col::deriveModuleShortPosition(llvmModule));
    }

    void ErrorReporter::addError(const std::string &source,
                                 const std::string &message,
                                 llvm::Function &llvmFunction) {
        addError(source, message, llvm2Col::deriveFunctionShortPosition(llvmFunction));
    }

    void ErrorReporter::addError(const std::string &source,
                                 const std::string &message,
                                 llvm::BasicBlock &llvmBlock) {
        addError(source, message, llvm2Col::deriveBlockShortPosition(llvmBlock));
    }

    void ErrorReporter::addError(const std::string &source,
                                 const std::string &message,
                                 llvm::Instruction &llvmInstruction) {
        addError(source, message, llvm2Col::deriveInstructionShortPosition(llvmInstruction));
    }

    bool ErrorReporter::hasErrors() {
        return ErrorReporter::errorCount > 0;
    }

    u_int32_t ErrorReporter::getErrorCount() {
        return ErrorReporter::errorCount;
    }
}