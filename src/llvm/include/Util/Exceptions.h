#ifndef PALLAS_EXCEPTIONS_H
#define PALLAS_EXCEPTIONS_H

#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

/**
 * Error handler for pallas. Contains exception types and a static ErrorReporter
 * class to which errors can be added from anywhere in the program. Before
 * attempting to serialize the buffer, pallas will check for errors. If there
 * are errors, pallas will present them and aboard the program.
 */
namespace pallas {
struct UnsupportedTypeException : public std::exception {
    UnsupportedTypeException(const llvm::Type &);

    [[nodiscard]] const char *what() const noexcept;

  private:
    std::string str;
};

class ErrorReporter {
  private:
    static u_int32_t errorCount;
    static u_int32_t warningCount;

  public:
    static void addError(const std::string &source, const std::string &message);

    static void addError(const std::string &source, const std::string &message,
                         const std::string &origin);

    static void addError(const std::string &source, const std::string &message,
                         llvm::Module &llvmModule);

    static void addError(const std::string &source, const std::string &message,
                         llvm::Function &llvmFunction);

    static void addError(const std::string &source, const std::string &message,
                         llvm::BasicBlock &llvmBlock);

    static void addError(const std::string &source, const std::string &message,
                         llvm::Instruction &llvmInstruction);

    static void addWarning(const std::string &source,
                           const std::string &message);

    static void addWarning(const std::string &source,
                           const std::string &message,
                           const std::string &origin);

    static void addWarning(const std::string &source,
                           const std::string &message,
                           llvm::Module &llvmModule);

    static void addWarning(const std::string &source,
                           const std::string &message,
                           llvm::Function &llvmFunction);

    static void addWarning(const std::string &source,
                           const std::string &message,
                           llvm::BasicBlock &llvmBlock);

    static void addWarning(const std::string &source,
                           const std::string &message,
                           llvm::Instruction &llvmInstruction);

    static bool hasErrors();

    static u_int32_t getErrorCount();
    static u_int32_t getWarningCount();
};
} // namespace pallas
#endif // PALLAS_EXCEPTIONS_H
