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

/**
 * Decode the identifier of a specification statement in the encoding used by
 * Pallas.
 * If the encoding is invalid, a nullopt is returned and errors are added.
 */
std::optional<SpecStatementType> getSpecStatementType(const llvm::MDNode *md);

/**
 * Decode a specification statement from the specification format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<SpecStatement> getSpecStatement(const llvm::MDNode *md);

/**
 * Decode a block of specification statements from the specification format of
 * Pallas. If the given metadata-node is not a valid encoding, an empty optional
 * is returned and errors are added.
 */
std::optional<SpecStatementBlock> getSpecStatementBlock(const llvm::MDNode *md);

/**
 * Decode a binding of a value from a yields argument to another ghost variable
 * that is encoded in the specification format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional
 * is returned and errors are added.
 */
std::optional<YieldsBinding> getYieldsBinding(const llvm::MDNode *md);

/**
 * Decode a block of yields-argument bindings that is encoded in the
 * specification format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<YieldsBindingBlock> getYieldsBindingBlock(const llvm::MDNode *md);

/**
 * Decode an assignment to a ghost variable that is encoded in the specification
 * format of Pallas.
 * Also used for given-bindings.
 * If the given metadata-node is not a valid encoding, an empty optional
 * is returned and errors are added.
 */
std::optional<GhostAssign> getGhostAssign(const llvm::MDNode *md);

/**
 * Decode a block of assignments to ghost variables that is encoded in the
 * specification format of Pallas.
 * Also used for given-bindings.
 * If the given metadata-node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<GhostAssignBlock> getGhostAssignBlock(const llvm::MDNode *md);

/**
 * Checks if a block of given-bindings is is attached to the given
 * instruction and returns it if it is present. Otherwise, a nullpointer is
 * returned
 */
llvm::MDNode *getGivenBindingBlockMD(llvm::Instruction &instr);

/**
 * Checks if a block of yields-bindings is is attached to the given
 * instruction and returns it if it is present. Otherwise, a nullpointer is
 * returned
 */
llvm::MDNode *getYieldsBindingBlockMD(llvm::Instruction &instr);

/**
 * Checks if a block of ghost-assignments is is attached to the given
 * instruction and returns it if it is present. Otherwise, a nullpointer is
 * returned
 */
llvm::MDNode *getGhostAssignBlockMD(llvm::Instruction &instr);

} // namespace pallas::irspec

#endif // PALLAS_SPEC_DECODING_H
