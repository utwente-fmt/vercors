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

struct LabeledColBlock {
    col::LlvmBasicBlock &bb;
    col::Block &block;
};

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

    /// All LLVM blocks mapped 1-to-1 to a COL block. This mapping is not direct
    /// in the sense that it uses the intermediate LabeledColBlock struct which
    /// contains both the COL label and COL block associated to the LLVM block
    std::unordered_map<llvm::BasicBlock *, LabeledColBlock>
        llvmBlock2LabeledColBlock;

    /// set of all COL blocks that have been completed. Completed meaning all
    /// instructions of the corresponding LLVM block have been transformed. This
    /// excludes possible future phi node back transformations.
    std::set<col::Block *> completedColBlocks;

    /// set of all COL blocks that we have started transforming.
    std::set<col::Block *> visitedColBlocks;

    /// map of assignments which should be added to the basic block when it is
    /// completed.
    std::unordered_multimap<col::Block *, col::Assign *> phiAssignBuffer;

    /// Map that is used to determine to which block a phi-assignment should be
    /// added to. Usually, this is the block that is referenced in the phi-node.
    /// However, in some cases we insert empty blocks to ensure that the
    /// phi-assignments are propagated correctly.
    /// In these cases, the phi-assignment should be added to the newly added
    /// block.
    /// The key of the map has the shape (from, toPhi) and
    /// maps to the new block to which the phi-assignment should be propagated
    /// (here [from] is the block from which the jump to the phi-instruction
    /// occurs, and [toPhi] is the block of the phi-instruction). Assumes that
    /// every key is ony inserted once.
    std::map<std::pair<col::Block *, col::Block *>, col::Block *>
        phiAssignmentTargetMap;

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
                                   col::Block &colBlock,
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
                                  col::Block &colBlock, col::Variable &varDecl);

    col::Assign &createPhiAssignment(Instruction &llvmInstruction,
                                     col::Block &colBlock,
                                     col::Variable &varDecl);

    col::Variable &getVariableMapEntry(llvm::Value &llvmValue, bool inPhiNode);

    /**
     * In many cases during transformation, it is not possible to derive whether
     * a COL block has yet been mapped and initialised. This is why we have a
     * get or set method which does the following" <ul> <li>If a mapping between
     * the given LLVM block and a COL block already exists, return the COL
     * block</li> <li>Else, initalise a new COL block in the buffer, add it to
     * the llvmBlock2LabeledColBlock lut and return the newly created COL
     * block</li>
     * </ul>
     *
     * @param llvmBlock
     * @return A LabeledColBlock struct to which this llvmBlock is mapped to.
     */
    LabeledColBlock &
    getOrSetLLVMBlock2LabeledColBlockEntry(BasicBlock &llvmBlock);

    /**
     * Adds a new, uninitialized LabeledColBlock to the body of the function
     * and returns a reference to this block.
     * The function is intended to be used for intermediary blocks that are
     * not present in the original llvm-module but are added during the
     * transformation as targets for propagating phi-assignments.
     * The passes instruction is used to construct the origin.
     */
    LabeledColBlock
    generateIntermediaryLabeledColBlock(llvm::Instruction &originInstruction);

    LabeledColBlock &visitLLVMBlock(BasicBlock &llvmBlock);

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

    /**
     * Mark COL Block as complete by adding it to the completedColBlocks set.
     * @param llvmBlock
     */
    void complete(col::Block &colBlock);

    /**
     * Indicates whether an llvmBlock has been fully transformed (excluding
     * possible phi node back transformations). Any completed block is also
     * visited.
     * @return true if block is in the completedColBlocks set, false otherwise.
     */
    bool isComplete(col::Block &colBlock);

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

    /**
     * Add a new target block for a phi-assignment to the map of phi-taget
     * blocks.
     * @param from The block from which the edge to the phi-instruction starts.
     * @param toPhi The block of the phi-instruction.
     * @param newBlock The new block that was inserted on the edge.
     */
    void addNewPhiAssignmentTargetBlock(col::Block &from, col::Block &toPhi,
                                        col::Block &newBlock);

    /**
     * Get the target-block for propagating a phi-assignment that is caused
     * by an edge between blocks [from] --> [to].
     * If a new block was inserted on this edge, the new block is returned.
     * Otherwise, [from] is returned.
     */
    col::Block *getTargetForPhiAssignment(col::Block &from, col::Block &to);
};

class FunctionBodyTransformerPass
    : public PassInfoMixin<FunctionBodyTransformerPass> {
  public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);
};
} // namespace pallas
#endif // PALLAS_FUNCTIONBODYTRANSFORMER_H
