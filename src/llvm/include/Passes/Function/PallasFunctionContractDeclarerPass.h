#ifndef PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H
#define PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H

#include "vct/col/ast/col.pb.h"

#include <memory>

#include <llvm/IR/Function.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/PassManager.h>

/**
 * Pass that transforms Pallas function contracts that are defined as
 * metadata that is attached to an LLVM-function into
 * LlvmfunctionContract objects.
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
    PreservedAnalyses run(Function &f, FunctionAnalysisManager &fam);

  private:
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
     * Tries to add a clause, that is represented by the given metadata-node, to
     * the given COL-contract.
     * Returns false if an error occurred (e.g. ill-formed metadata-node) and
     * true otherwise. In case of an error, an error is added to the
     * ErrorReporter.
     * parentFunc is the function to which the contract is attached.
     */
    bool addClauseToContract(col::ApplicableContract &contract,
                             Metadata *clauseOperand,
                             FunctionAnalysisManager &fam, Function &parentFunc,
                             unsigned int clauseNum,
                             const MDNode &contractSrcLoc);

    /**
     * Tries to extract the wrapper-function from the given metadata-node that
     * represents a clause of a Pallas contract (i.e. the operand at index 2
     * is expected to point to a function).
     * Also checks, if the function is marked as a wrapper-function.
     * Returns a nullptr id the function could not be extracted.
     * ctxFunc is used to build error messages.
     */
    Function *getWrapperFuncFromClause(MDNode &clause, Function &ctxFunc);

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
};
} // namespace pallas
#endif // PALLAS_PALLASFUNCTIONCONTRACTDECLARERPASS_H
