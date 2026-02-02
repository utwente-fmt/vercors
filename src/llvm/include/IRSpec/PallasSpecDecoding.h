#ifndef PALLAS_SPEC_DECODING_H
#define PALLAS_SPEC_DECODING_H

#include "PallasIRSpec.h"

#include <optional>

#include <llvm/IR/Metadata.h>

/**
 * Utils for decoding the Pallas specifications from metadata-nodes.
 */
namespace pallas::irspec {

/**
 * If the given metadata-node refers to an integer constant,
 * return the node as a ConstantInt. Otherwise, returns nullptr,
 */
const llvm::ConstantInt *asConstantInt(const llvm::Metadata *md);

/**
 * Checks if the provided mdNode is a valid encoding of a source location
 * according to the specification fromat used by Pallas.
 */
bool isWellformedPallasLocation(const llvm::MDNode *mdNode);

/**
 * If the given metadata node encodes a source location in the specification
 * format of Pallas, a SrcLoc is returned.
 * Otherwise, an an empty optional is returned and errors are added.
 */
std::optional<irspec::SrcLoc> getSrcLoc(const llvm::MDNode *md);

/**
 * If the given metadata node encodes a contract clause in the specification
 * format of Pallas, return it.
 * Otherwise, an empty optional is returned and errors are added.
 */
std::optional<ContractClause> getContractClause(const llvm::MDNode *md,
                                                bool hasImplicitArgs);

/**
 * If the given metadata node encodes a definition of a ghost argument
 * in the specification format of Pallas, returns it as a GhostArgDef.
 * Otherwise, an empty optional is returned and errors are added.
 */
std::optional<GhostArgDef> getGhostArgDef(const llvm::MDNode *md);

/**
 * If the given metadata node encodes a contract in the specification
 * format of Pallas, returns it as a SrcLoc.
 * Otherwise, an empty optional is returned and errors are added.
 */
std::optional<FunctionContract> getContract(const llvm::MDNode *md,
                                            bool externalOrGhost);

/**
 * Decode a loop invariant clause from the specification format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<LoopInvariantClause>
getLoopInvariantClause(const llvm::MDNode *md);

/**
 * Decode the loop-invariant block in the specification fromat of Pallas.
 * If the given metadata node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<LoopContract> getLoopContract(const llvm::MDNode *md);

} // namespace pallas::irspec

#endif // PALLAS_SPEC_DECODING_H
