#ifndef PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H
#define PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__

#include "IRSpec/PallasIRSpec.h"

#include <memory>

#include <llvm/IR/Function.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>

/**
 * Pass that transforms Pallas function contracts that are defined as
 * metadata that is attached to an LLVM-function into
 * LlvmfunctionContract objects.
 *
 * This is a module-pass instead of a function-pass to ensure that it is also
 * applied to function declarations which can be equiped with an
 * external contract.
 *
 * This pass expects to be run after the following passes
 * - FunctionContractDeclarer
 * - FunctionContractDeclarerPass
 *
 * The results can be accessed through FDCResult-objects using a
 * FunctionAnalysisManager.
 *
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class PallasFunctionContractDeclarerPass
    : public AnalysisInfoMixin<PallasFunctionContractDeclarerPass> {
  public:
    /**
     * Retrieves the LlvmfunctionDefinition object in the buffer from the
     * FDCResult object and sets the origin and data-fields of the contract.
     * The value-filed of the contract is left empty, because the
     * ApplicableContract is constructed directly.
     */
    PreservedAnalyses run(Module &m, ModuleAnalysisManager &mam);

  private:
    /**
     * Run the transformation on the given function.
     */
    void runOnFunction(Function &f, FunctionAnalysisManager &fam);

    /**
     * Initializes the given ApplicableContract so that it represents a
     * trivial contract (i.e. it only contains a requires-true-clause).
     */
    void initializeTrivialContract(col::ApplicableContract &contract,
                                   Function &f);

    /**
     * Adds an empty requires-clause (i.e. requires true;) to the given contract
     * if it does not already have a requires-clause.
     */
    void addEmptyRequires(col::ApplicableContract &contract, Function &f);

    /**
     * Adds an empty ensures-clause (i.e. ensures true;) to the given contract
     * if it does not already have a requires-clause.
     */
    void addEmptyEnsures(col::ApplicableContract &contract, Function &f);

    /**
     * Adds an empty context_everywhere (i.e. context_everywhere true;) to the
     * given contract if it does not already have a context_everywhere-clause.
     */
    void addEmptyContextEverywhere(col::ApplicableContract &contract,
                                   Function &f);

    /**
     * Adds an empty kernel_invariant (i.e. kernel_invariant true;) to the
     * given contract if it does not already have a kernel_invariant-clause.
     */
    void addEmptyKernelInvariant(col::ApplicableContract &contract,
                                 Function &f);

    /**
     * Tries to add a clause, that is represented by the given metadata-node, to
     * the given COL-contract.
     * Returns false if an error occurred (e.g. ill-formed metadata-node) and
     * true otherwise. In case of an error, an error is added to the
     * ErrorReporter.
     * parentFunc is the function to which the contract is attached.
     * The flag 'implicitArgs' indicates if the arguments of the
     * parent function are implicitly encoded in the contract (i.e. in external
     * or ghost contracts).
     */
    bool addClauseToContract(col::ApplicableContract &contract,
                             irspec::FunctionContract &irContract,
                             unsigned int clauseIdx,
                             FunctionAnalysisManager &fam, Function &parentFunc,
                             const bool implicitArgs);

    /**
     * Resolve the DIVariables from a given MD-nodes that encodes a contract-
     * clause into col-variables.
     */
    std::optional<SmallVector<col::Variable *, 8>>
    getContractArgs(const pallas::irspec::ContractClause &clause,
                    Function &parentFunc, FunctionAnalysisManager &fam);

    /**
     * Get the arguments for a call to a wrapper-function that is part of the
     * given parent-function's contract.
     */
    std::optional<SmallVector<col::Variable *, 8>>
    getExternalContractArgs(Function &parentFunc, FunctionAnalysisManager &fam);

    /**
     * Takes a function and a DIVariable that describes an argument of
     * the original source-function and attempts to map the DIVariable
     * to the corresponding argument of the llvm-function.
     * If the mapping isnot possible, a nullptr is returned.
     */
    Argument *mapDIVarToArg(Function &f, DIVariable &diVar);

    /**
     * Initializes the given predicate 'newPred' such that it represents a split
     * predicate that contains left and right.
     * Assumes that 'newPred' is already allocated, but uninitialized.
     * Assumes that 'left', 'right' and 'newPredOrigin' are owned by the caller.
     * After the function terminates, the ownership is transferred to 'newPred'.
     */
    void extendPredicate(col::AccountedPredicate *newPred,
                         col::Origin *newPredOrigin,
                         col::AccountedPredicate *left,
                         col::UnitAccountedPredicate *right);

    /**
     * Checks if the given llvm function is annotated with both, a VCLLVM and a
     * Pallas function contract. If so, an error is added to the ErrorReporter
     * and true is returned. Otherwise, false is returned.
     */
    bool hasConflictingContract(Function &f);

    /**
     * Determine the type of a ghost argument's definition.
     * isGivenArg = true  --> Assumed to be given-arg
     * isGivenArg = false --> Assumed to be yields-arg
     * If the type cannot be determined, returns nullptr and adds error.
     */
    llvm::Type *getGhostArgType(const irspec::FunctionContract &contract,
                                size_t argIdx, llvm::Function &f,
                                bool isGivenArg);

    /**
     * Initializes the given col-variable (colVar) based on theg given
     * ghost argument definition (gArgDef).
     */
    void transformGhostArg(const irspec::GhostArgDef &gArgDef,
                           col::Variable *colVar, llvm::Type &type, size_t idx,
                           llvm::Function &parentFunc);
};
} // namespace pallas
#endif // PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H
