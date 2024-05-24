#ifndef VCLLVM_PREFERREDNAMEDERIVER_H
#define VCLLVM_PREFERREDNAMEDERIVER_H

#include <llvm/IR/Value.h>
/**
 * Generators for VerCors origin objects preferredName fields for various LLVM Value types.
 *
 * For more info on VerCors origins see: https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2Col {
    std::string deriveOperandPreferredName(llvm::Value &llvmOperand);

    std::string deriveTypePreferredName(llvm::Type &llvmType);

    std::string deriveArgumentPreferredName(llvm::Argument &llvmArgument);

}
#endif //VCLLVM_PREFERREDNAMEDERIVER_H
