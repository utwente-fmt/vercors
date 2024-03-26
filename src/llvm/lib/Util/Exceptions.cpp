#include "Util/Exceptions.h"
#include "Origin/ShortPositionDeriver.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>
#include <sstream>

namespace pallas {
UnsupportedTypeException::UnsupportedTypeException(const llvm::Type &type) {
    llvm::raw_string_ostream output(str);
    output << "Type '" << type << "' not supported";
}

[[nodiscard]] const char *UnsupportedTypeException::what() const noexcept {
    return str.c_str();
}

u_int32_t ErrorReporter::errorCount;
u_int32_t ErrorReporter::warningCount;

void ErrorReporter::addError(const std::string &source,
                             const std::string &message) {
    llvm::errs() << "[ERROR] [pallas] [" << source << "] " << message << "\n\n";
    ErrorReporter::errorCount++;
}

void ErrorReporter::addError(const std::string &source,
                             const std::string &message,
                             const std::string &origin) {
    llvm::errs() << "[ERROR] [pallas] [" << source << "] " << message
                 << " @\n  " << origin << "\n\n";
    ErrorReporter::errorCount++;
}

void ErrorReporter::addError(const std::string &source,
                             const std::string &message,
                             llvm::Module &llvmModule) {
    addError(source, message, llvm2col::deriveModuleShortPosition(llvmModule));
}

void ErrorReporter::addError(const std::string &source,
                             const std::string &message,
                             llvm::Function &llvmFunction) {
    addError(source, message,
             llvm2col::deriveFunctionShortPosition(llvmFunction));
}

void ErrorReporter::addError(const std::string &source,
                             const std::string &message,
                             llvm::BasicBlock &llvmBlock) {
    addError(source, message, llvm2col::deriveBlockShortPosition(llvmBlock));
}

void ErrorReporter::addError(const std::string &source,
                             const std::string &message,
                             llvm::Instruction &llvmInstruction) {
    addError(source, message,
             llvm2col::deriveInstructionShortPosition(llvmInstruction));
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message) {
    llvm::errs() << "[WARN] [pallas] [" << source << "] " << message << "\n\n";
    ErrorReporter::warningCount++;
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message,
                               const std::string &origin) {
    llvm::errs() << "[WARN] [pallas] [" << source << "] " << message << " @\n  "
                 << origin << "\n\n";
    ErrorReporter::warningCount++;
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message,
                               llvm::Module &llvmModule) {
    addWarning(source, message,
               llvm2col::deriveModuleShortPosition(llvmModule));
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message,
                               llvm::Function &llvmFunction) {
    addWarning(source, message,
               llvm2col::deriveFunctionShortPosition(llvmFunction));
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message,
                               llvm::BasicBlock &llvmBlock) {
    addWarning(source, message, llvm2col::deriveBlockShortPosition(llvmBlock));
}

void ErrorReporter::addWarning(const std::string &source,
                               const std::string &message,
                               llvm::Instruction &llvmInstruction) {
    addWarning(source, message,
               llvm2col::deriveInstructionShortPosition(llvmInstruction));
}

bool ErrorReporter::hasErrors() { return ErrorReporter::errorCount > 0; }

u_int32_t ErrorReporter::getErrorCount() { return ErrorReporter::errorCount; }
u_int32_t ErrorReporter::getWarningCount() {
    return ErrorReporter::warningCount;
}
} // namespace pallas
