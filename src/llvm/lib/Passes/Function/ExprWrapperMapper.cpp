#include "Passes/Function/ExprWrapperMapper.h"
#include "Passes/Function/FunctionDeclarer.h"

#include "Origin/OriginProvider.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Metadata.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Function::ExprWrapperMapper";

using namespace llvm;
namespace col = vct::col::ast;

/*
 * EWMResult
 */

EWMResult::EWMResult(llvm::Function *parentFunc,
                     std::optional<PallasWrapperContext> ctx)
    : parentFunc(parentFunc), context(ctx) {}

llvm::Function *EWMResult::getParentFunc() { return parentFunc; }

std::optional<PallasWrapperContext> EWMResult::getContext() { return context; }

/*
 * ExpressionWrapperMapper
 */

AnalysisKey ExprWrapperMapper::Key;

ExprWrapperMapper::Result ExprWrapperMapper::run(Function &F,
                                                 FunctionAnalysisManager &FAM) {
    if (!utils::isPallasExprWrapper(F))
        return EWMResult(nullptr, std::nullopt);
    auto *llvmModule = F.getParent();

    // For all functions in the current module, check if they reference this
    // wrapper-function in a specification.
    for (Function &parentF : llvmModule->functions()) {
        // Skip wrapper-functions and intrinsics
        if (utils::isPallasExprWrapper(parentF) || parentF.isIntrinsic()) {
            continue;
        }

        // If the function has a pallas-contract, check all clauses
        if (utils::hasPallasContract(parentF)) {
            auto *contract =
                parentF.getMetadata(constants::PALLAS_FUNC_CONTRACT);
            // For all clauses, check if they reference the wrapper function
            auto numOps = contract->getNumOperands();
            unsigned int clauseIdx = 2;
            for (clauseIdx = 2; clauseIdx < numOps; ++clauseIdx) {
                //  Try to get the third operand as a function
                auto *clause =
                    dyn_cast<MDNode>(contract->getOperand(clauseIdx).get());
                if (clause == nullptr || clause->getNumOperands() < 3)
                    continue;
                auto *clauseWrapper = getWrapperFromFContractClause(*clause);
                // Check if the wrapper-function in the clause is the function
                // that we are looking for.
                if (clauseWrapper != nullptr && clauseWrapper == &F) {
                    // Determine the context in which the wrapper is used.
                    auto ctx = getContextForFContractClause(*clause);
                    return EWMResult(&parentF, ctx);
                }
            }
        }

        // Check all loop-contracts
        LoopInfo &loopInfo = FAM.getResult<LoopAnalysis>(parentF);
        auto loops = loopInfo.getLoopsInPreorder();
        for (Loop *loop : loops) {
            if (loop == nullptr)
                continue;
            // Get loop-contract
            llvm::MDNode *contractMD =
                pallas::utils::getPallasLoopContract(*loop);
            if (contractMD == nullptr)
                continue;
            // For all invariants, check if they refer to F
            auto numOps = contractMD->getNumOperands();
            for (unsigned int invIdx = 2; invIdx < numOps; ++invIdx) {
                // Cast operand into MDNode
                auto *invMD = dyn_cast_if_present<MDNode>(
                    contractMD->getOperand(invIdx).get());
                if (invMD == nullptr)
                    continue;
                // Get wrapper function
                auto *wFunc = pallas::utils::getWrapperFromLoopInv(*invMD);
                if (wFunc != nullptr && wFunc == &F) {
                    return EWMResult(&parentF,
                                     PallasWrapperContext::LoopContractInv);
                }
            }
        }
    }
    return EWMResult(nullptr, std::nullopt);
}

Function *
ExprWrapperMapper::getWrapperFromFContractClause(const llvm::MDNode &clause) {
    auto *clauseWrapperMD =
        dyn_cast<ValueAsMetadata>(clause.getOperand(2).get());
    if (clauseWrapperMD == nullptr)
        return nullptr;
    return dyn_cast_if_present<Function>(clauseWrapperMD->getValue());
}

std::optional<PallasWrapperContext>
ExprWrapperMapper::getContextForFContractClause(const llvm::MDNode &clause) {
    std::optional<PallasWrapperContext> ctx = std::nullopt;
    // Attempt to get string with clause-type from first operand of the clause.
    if (auto *fClauseTMD = dyn_cast<MDString>(clause.getOperand(0).get())) {
        auto clauseTStr = fClauseTMD->getString().str();
        if (clauseTStr == pallas::constants::PALLAS_REQUIRES) {
            ctx = PallasWrapperContext::FuncContractPre;
        } else if (clauseTStr == pallas::constants::PALLAS_ENSURES) {
            ctx = PallasWrapperContext::FuncContractPost;
        }
    }
    return ctx;
}

} // namespace pallas
