#ifndef PALLAS_FUNCTIONDECLARER_H
#define PALLAS_FUNCTIONDECLARER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>

/**
 * Pass that creates a signature for a LlvmfunctionDefinition in COL and exposes
 * an FDResult object that binds the the LLVM IR Function to a
 * LlvmfunctionDefinition COL object. The actual function implementation is
 * transformed by the FunctionBodyTransformer pass.
 *
 * The pass is twofold: it has an analysis pass (FunctionDeclarer) that merely
 * creates objects in the buffer and adds them to the associated result object.
 * This way, the result object of this pass can be queried by other passes in
 * order to retrieve the relevant COL nodes associated to this LLVM function.
 *
 * The second pass is a regular function pass (FunctionDeclarerPass) that
 * finishes the transformation started by the FunctionDeclarer analysis pass.
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

/// wrapper struct for a COL scope and block. Intended use is the block to be
/// declared in the scope.
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
    /// contains the 1-to-1 mapping from LLVM function arguments to COL
    /// variables that are used as function arguments.
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

  public:
    using Result = FDResult;

    /**
     * Creates a COL LlvmfunctionDefinition in the buffer, including a function
     * scope and body and their origins. It maps the corresponding LLVM Function
     * to the created COL LlvmfunctionDefinition.
     *
     * Additionally, it creates the function arguments (COL variables) in the
     * buffer and maps the corresponding LLVM arguments to the created COL
     * arguments.
     *
     * @param F
     * @param FAM
     * @return
     */
    Result run(Function &F, FunctionAnalysisManager &FAM);
};

class FunctionDeclarerPass : public AnalysisInfoMixin<FunctionDeclarerPass> {
  public:
    /**
     * Completes the function definition transformation by adding a return type
     * to the COL LlvmfunctionDefinition
     *
     * @param F
     * @param FAM
     * @return
     */
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
};
} // namespace pallas
#endif // PALLAS_FUNCTIONDECLARER_H
