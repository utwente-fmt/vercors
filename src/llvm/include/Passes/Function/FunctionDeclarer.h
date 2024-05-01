#ifndef VCLLVM_FUNCTIONDECLARER_H
#define VCLLVM_FUNCTIONDECLARER_H

#include <llvm/IR/PassManager.h>
#include "vct/col/ast/col.pb.h"

/**
 * Pass that creates a signature for a LLVMFunctionDefinition in COL and exposes an FDResult object that
 * binds the the LLVM IR Function to a LLVMFunctionDefinition COL object. The actual function implementation is
 * transformed by the FunctionBodyTransformer pass.
 *
 * The pass is twofold: it has an analysis pass (FunctionDeclarer) that merely creates objects in the buffer and adds
 * them to the associated result object. This way, the result object of this pass can be queried by other passes in order
 * to retrieve the relevant COL nodes associated to this LLVM function.
 *
 * The second pass is a regular function pass (FunctionDeclarerPass) that finishes the transformation started by the
 * FunctionDeclarer analysis pass.
 */
namespace vcllvm {
    using namespace llvm;
    namespace col = vct::col::ast;

    /// wrapper struct for a COL scope and block. Intended use is the block to be declared in the scope.
    struct ColScopedFuncBody {
        col::Scope *scope;
        col::Block *block;
    };

    class FDResult {
        friend class FunctionDeclarer;

    private:
        col::LlvmFunctionDefinition &associatedColFuncDef;
        ColScopedFuncBody associatedScopedColFuncBody;
        int64_t functionId;
        /// contains the 1-to-1 mapping from LLVM function arguments to COL variables that are used as function
        /// arguments.
        std::unordered_map<llvm::Argument *, col::Variable *> funcArgMap;

        void addFuncArgMapEntry(llvm::Argument &llvmArg, col::Variable &colArg);

    public:
        explicit FDResult(col::LlvmFunctionDefinition &colFuncDef,
                          ColScopedFuncBody associatedScopedColFuncBody,
                          int64_t functionId);

        col::LlvmFunctionDefinition &getAssociatedColFuncDef();

        ColScopedFuncBody getAssociatedScopedColFuncBody();

        col::Variable &getFuncArgMapEntry(llvm::Argument &arg);

        int64_t &getFunctionId();
    };

    class FunctionDeclarer : public AnalysisInfoMixin<FunctionDeclarer> {
        friend AnalysisInfoMixin<FunctionDeclarer>;
        static AnalysisKey Key;
    private:
        std::shared_ptr<col::Program> pProgram;
    public:
        using Result = FDResult;

        explicit FunctionDeclarer(std::shared_ptr<col::Program> pProgram);

        /**
         * Creates a COL LlvmFunctionDefinition in the buffer, including a function scope and body and their origins.
         * It maps the corresponding LLVM Function to the created COL LlvmFunctionDefinition.
         *
         * Additionally, it creates the function arguments (COL variables) in the buffer and maps the corresponding
         * LLVM arguments to the created COL arguments.
         *
         * @param F
         * @param FAM
         * @return
         */
        Result run(Function &F, FunctionAnalysisManager &FAM);

    };

    class FunctionDeclarerPass : public AnalysisInfoMixin<FunctionDeclarerPass> {
    private:
        std::shared_ptr<col::Program> pProgram;
    public:
        explicit FunctionDeclarerPass(std::shared_ptr<col::Program> pProgram);
        /**
         * Completes the function definition transformation by adding a return type to the COL LLVMFunctionDefinition
         *
         * @param F
         * @param FAM
         * @return
         */
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
    };
}
#endif //VCLLVM_FUNCTIONDECLARER_H