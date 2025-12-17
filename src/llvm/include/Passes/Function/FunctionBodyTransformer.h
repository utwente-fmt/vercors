#ifndef PALLAS_FUNCTIONBODYTRANSFORMER_H
#define PALLAS_FUNCTIONBODYTRANSFORMER_H

#include <map>
#include <utility>

#include <llvm/Analysis/LoopInfo.h>

#include "FunctionDeclarer.h"
/**
 * The FunctionBodyTransformer that transforms LLVM blocks and instructions into
 * suitable VerCors COL abstractions.
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

/**
 * The FunctionCursor is a stateful utility class to transform a LLVM function
 * body to a COL function body.
 */
class FunctionCursor {
    friend class FunctionBodyTransformerPass;

  private:
    col::Scope &functionScope;

    col::Block &functionBody;

    llvm::Function &llvmFunction;

    /// Gives access to all other analysis passes ran by pallas as well as
    /// existing LLVM analysis passes (i.e. loop analysis).
    llvm::FunctionAnalysisManager &FAM;

    /// Most LLVM instructions are transformed to a COL assignment to a COL
    /// variable. The resulting end product is a 1-to-1 mapping from and LLVM
    /// Value to a COL variable. The generic LLVM Value was chosen to also
    /// include function arguments in the lut.
    std::unordered_map<llvm::Value *, col::Variable *> variableMap;

    /// All LLVM blocks mapped 1-to-1 to a COL block.
    std::unordered_map<llvm::BasicBlock *, col::LlvmBasicBlock &>
        llvmBlock2ColBlock;

    /// set of all COL blocks that we have started transforming.
    std::set<col::LlvmBasicBlock *> visitedColBlocks;

    /// Almost always when adding a variable to the variableMap, some extra
    /// processing is required which is why this method is private as to not
    /// accidentally use it outside the functionCursor
    void addVariableMapEntry(llvm::Value &llvmValue, col::Variable &colVar);

  public:
    explicit FunctionCursor(col::Scope &functionScope, col::Block &functionBody,
                            llvm::Function &llvmFunction,
                            llvm::FunctionAnalysisManager &FAM);

    const col::Scope &getFunctionScope();

    /**
     * declares variable in the function scope
     * @param llvmInstruction
     * @return the created variable declaration
     */
    col::Variable &declareVariable(Instruction &llvmInstruction,
                                   Type *llvmPointerType = nullptr);

    /**
     * Functionality is twofold:
     * <ol>
     *  <li>Creates a variable declaration in the function scope (declare
     * variable)</li> <li>Creates an assignment in the provided colBlock</li>
     * </ol>
     * @param llvmInstruction
     * @param colBlock
     * @return The created col assignment
     */
    col::Assign &
    createAssignmentAndDeclaration(Instruction &llvmInstruction,
                                   col::LlvmBasicBlock &colBlock,
                                   Type *llvmPointerType = nullptr);

    /**
     * Creates an assignment in the provided colBlock referencing the provided
     * variable declaration
     *
     * @param llvmInstruction
     * @param colBlock
     * @param varDecl
     * @return the created col assignment
     */
    col::Assign &createAssignment(Instruction &llvmInstruction,
                                  col::LlvmBasicBlock &colBlock,
                                  col::Variable &varDecl);

    col::Assign &createPhiAssignment(Instruction &llvmInstruction,
                                     col::LlvmBasicBlock &colBlock,
                                     col::Variable &varDecl);

    col::Variable &getVariableMapEntry(llvm::Value &llvmValue, bool inPhiNode);

    /**
     * In many cases during transformation, it is not possible to derive whether
     * a COL block has yet been mapped and initialised. This is why we have a
     * get or set method which does the following" <ul> <li>If a mapping between
     * the given LLVM block and a COL block already exists, return the COL
     * block</li> <li>Else, initalise a new COL block in the buffer, add it to
     * the llvmBlock2ColBlock lut and return the newly created COL
     * block</li>
     * </ul>
     *
     * @param llvmBlock
     * @return Reference to col-block to which this llvmBlock is mapped to.
     */
    col::LlvmBasicBlock &getOrSetLLVMBlock2ColBlockEntry(BasicBlock &llvmBlock);

    col::LlvmBasicBlock &visitLLVMBlock(BasicBlock &llvmBlock);

    llvm::FunctionAnalysisManager &getFunctionAnalysisManager();

    /**
     * Indicates whether a LLVM block has been visited (i.e. whether a mapping
     * exists to a COL block). Note that does not mean that it has been fully
     * transformed. For that see the isComplete
     *
     * @param llvmBlock
     * @return
     */
    bool isVisited(llvm::BasicBlock &llvmBlock);

    LoopInfo &getLoopInfo();

    LoopInfo &getLoopInfo(llvm::Function &otherLLVMFunction);

    /**
     * Retrieve the FunctionDeclarerPass analysis result from the function this
     * FunctionCursor is associated with by querying the
     * FunctionAnalysisManager.
     * @return
     */
    FDResult &getFDResult();

    /**
     * Retrieve the FunctionDeclarerPass analysis result from a function in the
     * current program by querying the FunctionAnalysisManager.
     * @param otherLLVMFunction
     * @return
     */
    FDResult &getFDResult(llvm::Function &otherLLVMFunction);
};

class FunctionBodyTransformerPass
    : public PassInfoMixin<FunctionBodyTransformerPass> {
  public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
};
} // namespace pallas
#endif // PALLAS_FUNCTIONBODYTRANSFORMER_H
