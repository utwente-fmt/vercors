#ifndef PALLAS_SPEC_DECODING_H
#define PALLAS_SPEC_DECODING_H

#include "PallasIRSpec.h"

#include <optional>

#include <llvm/Analysis/LoopInfo.h>
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
 * Decodes a metadata node that contains a mapping of ghost variables to
 * arguments of a wrapper function.
 * Returns false if the given MDNode is not a valid encoding and adds an error.
 */
bool decodeWArgToGhostMapping(const llvm::MDNode *md,
                              WrapperArgGhostMap &mapping,
                              const std::string &errMsg);

/**
 * Decodes a metadata node that contains a mapping of variables from the parent
 * function to arguments of a wrapper function. Returns false if the given
 * MDNode is not a valid encoding and adds an error.
 */
bool decodeWArgToVarMapping(const llvm::MDNode *md, WrapperArgVarMap &mapping,
                            const std::string &errMsg);

/**
 * Decodes the mappings of ghost and regular variables to the arguments of a 
 * wrapper-function. 
 * Assumes that the passed MD-node contains the mapping of 
 * - given-args as operand 3
 * - yields-args as operand 4
 * - regular variables as operand 5
 * Returns false if the given MDNode is not a valid encoding and adds an error.
 */
bool decodeWrapperArgMapping(const llvm::MDNode &md,
                             WrappedSpecElement &specElem,
                             const std::string &errMsg);

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
std::optional<ContractClause> getContractClause(const llvm::MDNode *md);

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
std::optional<FunctionContract> getContract(const llvm::MDNode *md);

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
 * Decode a binding of a value to a given-argument that is encoded in the specification
 * format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional
 * is returned and errors are added.
 */
std::optional<GivenBinding> getGivenBinding(const llvm::MDNode *md);

/**
 * Decode a block of bindings to given arguments that is encoded in the
 * specification format of Pallas.
 * If the given metadata-node is not a valid encoding, an empty optional is
 * returned and errors are added.
 */
std::optional<GivenBindingBlock> getGivenBindingBlock(const llvm::MDNode *md);

/**
 * If the given loop has a loop-contract, a pointer to the MDNode
 * that represents the contract is returned.
 * If no loop contract is present, a nullptr is returned.
 */
llvm::MDNode *getLoopContractMD(const llvm::Loop &llvmLoop);

/**
 * Checks if a block if specification-statements is attached to the given
 * instruction and returns it if it is present. Otherwise, a nullpointer is
 * returned
 */
llvm::MDNode *getStmntBlockMD(llvm::Instruction &instr);

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
 * Checks if the given function is labeled as a function from the pallas
 * specification-library.
 * If so, returns an optinal that contains the string-identifier of the 
 * spec-lib function. Otherwise, an empty optional is returned.
 */
std::optional<std::string> isPallasSpecLib(const llvm::Function &f);

/**
 * Checks if the given function has a metadata-node that is labeled as a
 * Pallas function contract.
 */
bool hasPallasContract(const llvm::Function &f);

/**
 * Checks if the given function has a metadata-node that is labeled as an
 * external Pallas contract.
 */
bool hasExternalPallasContract(const llvm::Function &f);

/**
 * If f has an external or a normal pallas-contract, return this contract.
 */
llvm::MDNode *getPallasContract(const llvm::Function &f);

/**
 * Checks if the given llvm function is marked as an expression wrapper of a
 * pallas specification.
 */
bool isPallasExprWrapper(const llvm::Function &f);

/**
 * Checks if the given llvm function is marked as ghost wrapper.
 */
bool isPallasGhostWrapper(const llvm::Function &f);

/**
 * Checks if the given llvm function is marked as a predicate definition.
 */
bool isPallasPredDef(const llvm::Function &f);

/**
 * Checks if the given function that represents a pallas predicate definition
 * is marked as inline or not.
 * If the function is not a valid predicate or if the metadata is malformed,
 * a nullopt is returned.
 */
std::optional<bool> isPallasPredInline(const llvm::Function &f);

} // namespace pallas::irspec

#endif // PALLAS_SPEC_DECODING_H
