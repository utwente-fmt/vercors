#ifndef VCLLVM_FUNCTIONCONTRACTDECLARER_H
#define VCLLVM_FUNCTIONCONTRACTDECLARER_H

#include <llvm/IR/PassManager.h>
#include "col.pb.h"

/**
 * Pass that adds an LLVMFunctionContract to its corresponding LLVMFunctionDefinition in the presence
 * of a contract metadata node. The resulting FDCResult class can be used by a FunctionAnalysisManager to access the
 * created contract and add named references to the contract (e.g. map functions arguments string representations to COL
 * variables representing these same arguments).
 *
 * The pass is twofold: it has an analysis pass (FunctionContractDeclarer) that merely creates objects in the buffer and
 * adds them to the associated result object. This way, the result object of this pass can be queried by other passes in
 * order to retrieve the relevant COL nodes associated to this LLVM function.
 *
 * The second pass is a regular function pass (FunctionContractDeclarerPass) that finishes the transformation started by
 * the FunctionContractDeclarer analysis pass.
 */
namespace vcllvm {
    using namespace llvm;
    namespace col = vct::col::serialize;

    class FDCResult {
    private:
        col::LlvmFunctionContract &associatedColFuncContract;
    public:
        explicit FDCResult(col::LlvmFunctionContract &colFuncContract);

        col::LlvmFunctionContract &getAssociatedColFuncContract();
    };

    class FunctionContractDeclarer : public AnalysisInfoMixin<FunctionContractDeclarer> {
        friend AnalysisInfoMixin<FunctionContractDeclarer>;
        static AnalysisKey Key;
    private:
        std::shared_ptr<col::Program> pProgram;
    public:
        using Result = FDCResult;

        explicit FunctionContractDeclarer(std::shared_ptr<col::Program> pProgram);

        /**
         * Merely creates a COL LlvmFunctionDefinition object in the buffer and sets it in a FDCResult object.
         * @param F
         * @param FAM
         * @return
         */
        Result run(Function &F, FunctionAnalysisManager &FAM);
    };

    class FunctionContractDeclarerPass : public AnalysisInfoMixin<FunctionContractDeclarerPass> {
    private:
        std::shared_ptr<col::Program> pProgram;
    public:
        explicit FunctionContractDeclarerPass(std::shared_ptr<col::Program> pProgram);

        /**
         * Retrieves the LlvmFunctionDefinition object in the buffer from the FDCResult object and sets the origin and
         * string value of the contract.
         * @param F
         * @param FAM
         * @return
         */
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
    };
}
#endif //VCLLVM_FUNCTIONCONTRACTDECLARER_H
