#ifndef PALLAS_MD_H
#define PALLAS_MD_H

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>
#include <optional>
#include <string>

/**
 * Utils for working with the metadata-node of pallas specifications.
 */
namespace pallas::utils {

/**
 * Checks if the given function has a metadata-node that is labeled as a
 * VCLLVM contract.
 */
bool hasVcllvmContract(const llvm::Function &f);

} // namespace pallas::utils

#endif // PALLAS_MD_H
