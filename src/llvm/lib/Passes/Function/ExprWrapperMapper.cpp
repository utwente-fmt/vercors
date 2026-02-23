#include "Passes/Function/ExprWrapperMapper.h"

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__

#include "IRSpec/PallasSpecDecoding.h"
#include "Util/Constants.h"
#include "Util/PallasMD.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Function::ExprWrapperMapper";

using namespace llvm;
namespace col = vct::col::ast;

/*
 * EWMResult
 */

EWMResult::EWMResult(llvm::Function *parentFunc) : parentFunc(parentFunc) {}

llvm::Function *EWMResult::getParentFunc() { return parentFunc; }

/*
 * ExpressionWrapperMapper
 */

AnalysisKey ExprWrapperMapper::Key;

ExprWrapperMapper::Result ExprWrapperMapper::run(Function &F,
                                                 FunctionAnalysisManager &FAM) {
    if (!(irspec::isPallasExprWrapper(F) || irspec::isPallasGhostWrapper(F)))
        return EWMResult(nullptr);
    auto *llvmModule = F.getParent();

    // For all functions in the current module, check if they reference this
    // wrapper-function in a specification.
    for (Function &parentF : llvmModule->functions()) {
        // Skip wrapper-functions and intrinsics
        if (irspec::isPallasExprWrapper(parentF) ||
            irspec::isPallasGhostWrapper(parentF) || parentF.isIntrinsic() ||
            irspec::isPallasSpecLib(parentF)) {
            continue;
        }

        // If the function has a pallas-contract, check all clauses
        if (auto *contrMD = irspec::getPallasContract(parentF)) {
            auto contract = irspec::getContract(contrMD);

            for (auto &clause : contract->clauses) {
                if (&clause.getWrapper() == &F)
                    return EWMResult(&parentF);
            }
        }

        if (parentF.isDeclaration())
            continue;

        // Check all loop-contracts
        LoopInfo &loopInfo = FAM.getResult<LoopAnalysis>(parentF);
        auto loops = loopInfo.getLoopsInPreorder();
        for (Loop *loop : loops) {
            if (loop == nullptr)
                continue;
            // Get loop-contract
            auto *contractMD = irspec::getLoopContractMD(*loop);
            if (contractMD == nullptr)
                continue;
            // Check all invariants
            auto contr = irspec::getLoopContract(contractMD);
            if (!contr.has_value())
                return EWMResult(nullptr);

            for (auto &inv : contr->clauses) {
                if (&inv.getWrapper() == &F)
                    return EWMResult(&parentF);
            }
        }

        // Check specs that are attached to instructions
        for (auto it = llvm::inst_begin(parentF), end = llvm::inst_end(parentF);
             it != end; ++it) {
            llvm::Instruction *inst = &*it;

            // Check blocks of specification-statements
            if (auto *specBlockMD = irspec::getStmntBlockMD(*inst)) {
                auto stmntBlock = irspec::getSpecStatementBlock(specBlockMD);
                if (!stmntBlock.has_value())
                    return EWMResult(nullptr);
                for (auto &stmnt : stmntBlock->statements) {
                    if (&stmnt.getWrapper() == &F)
                        return EWMResult(&parentF);
                }
            }

            // Check given-assignments
            if (const auto *gBindMD = irspec::getGivenBindingBlockMD(*inst)) {
                auto bBlock = irspec::getGivenBindingBlock(gBindMD);
                if (!bBlock.has_value())
                    return EWMResult(nullptr);
                for (auto &b : bBlock->bindings) {
                    if (&b.getWrapper() == &F)
                        return EWMResult(&parentF);
                }
            }
        }
    }
    return EWMResult(nullptr);
}

} // namespace pallas
