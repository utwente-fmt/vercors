#ifndef PALLAS_EXPRWRAPPERMAPPER_H
#define PALLAS_EXPRWRAPPERMAPPER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>
#include <optional>

/**
 * Analysis-pass that maps functions that represent expression wrappers in a
 * Pallas specification to the function to whose specification they belong.
 */
namespace pallas {

enum PallasWrapperContext {
    FuncContractPre,
    FuncContractPost,
    LoopContractInv
};

class EWMResult {
  private:
    llvm::Function *parentFunc;
    std::optional<PallasWrapperContext> context;

  public:
    explicit EWMResult(llvm::Function *parentFunc,
                       std::optional<PallasWrapperContext> ctx);

    llvm::Function *getParentFunc();

    std::optional<PallasWrapperContext> getContext();
};

class ExprWrapperMapper : public llvm::AnalysisInfoMixin<ExprWrapperMapper> {
    friend llvm::AnalysisInfoMixin<ExprWrapperMapper>;
    static llvm::AnalysisKey Key;

  public:
    using Result = EWMResult;

    /**
     * Maps functions that represent a Pallas expression wrapper to the function
     * to whose specification they belong to.
     * If a function does not belong to the contract of any function,
     * the result contains a nullpointer.
     */
    Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);

  private:
    /**
     * Attempts to get the wrapper-function from the given MDNode which
     * represents a clause of a Pallas function contract.
     */
    llvm::Function *getWrapperFromFContractClause(const llvm::MDNode &clause);

    std::optional<PallasWrapperContext>
    getContextForFContractClause(const llvm::MDNode &clause);
};

} // namespace pallas
#endif // PALLAS_EXPRWRAPPERMAPPER_H
