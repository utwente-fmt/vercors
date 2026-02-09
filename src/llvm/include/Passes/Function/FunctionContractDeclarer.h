#ifndef PALLAS_FUNCTIONCONTRACTDECLARER_H
#define PALLAS_FUNCTIONCONTRACTDECLARER_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
#include <llvm/IR/PassManager.h>

#include "IRSpec/PallasIRSpec.h"

/**
 * Pass that adds an LlvmfunctionContract to its corresponding
 * LlvmfunctionDefinition in the presence of a contract metadata node. The
 * resulting FDCResult class can be used by a FunctionAnalysisManager to access
 * the created contract and add named references to the contract (e.g. map
 * functions arguments string representations to COL variables representing
 * these same arguments).
 *
 * The pass is twofold: it has an analysis pass (FunctionContractDeclarer) that
 * merely creates objects in the buffer and adds them to the associated result
 * object. This way, the result object of this pass can be queried by other
 * passes in order to retrieve the relevant COL nodes associated to this LLVM
 * function.
 *
 * The second pass is a regular function pass (FunctionContractDeclarerPass)
 * that finishes the transformation started by the FunctionContractDeclarer
 * analysis pass.
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class FDCResult {
  private:
    col::LlvmFunctionContract &associatedColFuncContract;

    std::optional<irspec::FunctionContract> associatedIRContract = std::nullopt;

    std::unordered_map<const irspec::GhostArgDef *, col::Variable *>
        ghostArgMap;

  public:
    explicit FDCResult(col::LlvmFunctionContract &colFuncContract);

    void setIRContract(irspec::FunctionContract irContract);

    const irspec::FunctionContract *getIRContract();

    void addGhostArgMapEntry(const irspec::GhostArgDef &arg,
                             col::Variable &colVar);

    col::Variable *getGhostArgMapEntry(const irspec::GhostArgDef &arg);

    col::Variable *getGhostArgByName(const std::string &argName);

    llvm::SmallVector<col::Variable *> getGhostVars();

    col::LlvmFunctionContract &getAssociatedColFuncContract();
};

class FunctionContractDeclarer
    : public AnalysisInfoMixin<FunctionContractDeclarer> {
    friend AnalysisInfoMixin<FunctionContractDeclarer>;
    static AnalysisKey Key;

  public:
    using Result = FDCResult;

    /**
     * Merely creates a COL LlvmfunctionDefinition object in the buffer and sets
     * it in a FDCResult object.
     * @param F
     * @param FAM
     * @return
     */
    Result run(Function &F, FunctionAnalysisManager &FAM);
};

class FunctionContractDeclarerPass
    : public AnalysisInfoMixin<FunctionContractDeclarerPass> {
  public:
    /**
     * Retrieves the LlvmfunctionDefinition object in the buffer from the
     * FDCResult object and sets the origin and string value of the contract.
     * @param F
     * @param FAM
     * @return
     */
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
};
} // namespace pallas
#endif // PALLAS_FUNCTIONCONTRACTDECLARER_H
