#ifndef PALLAS_PREFERREDNAMEDERIVER_H
#define PALLAS_PREFERREDNAMEDERIVER_H

#include <llvm/IR/Value.h>
#include <llvm/Support/AtomicOrdering.h>
/**
 * Generators for VerCors origin objects preferredName fields for various LLVM
 * Value types.
 *
 * For more info on VerCors origins see:
 * https://github.com/utwente-fmt/vercors/discussions/884
 */
namespace llvm2col {
std::string deriveOperandPreferredName(llvm::Value &llvmOperand);

std::string deriveTypePreferredName(llvm::Type &llvmType);

std::string
deriveMemoryOrderingPreferredName(llvm::AtomicOrdering &llvmOrdering);

std::string deriveArgumentPreferredName(llvm::Argument &llvmArgument);
} // namespace llvm2col
#endif // PALLAS_PREFERREDNAMEDERIVER_H
