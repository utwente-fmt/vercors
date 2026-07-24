#include "Transform/WrapperCallTransform.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasWrapperUtils.h"

#include <llvm/Support/Casting.h>

const std::string SOURCE_LOC = "Transform::WrapperCallTransform";

namespace llvm2col {

namespace col = vct::col::ast;
namespace utils = pallas::utils;

namespace {

void printError(llvm::Function &f, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed specification: " + msg, f);
}

void printError(llvm::Instruction &inst, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed specification: " + msg, inst);
}

void printError(const llvm::Metadata &md, std::string msg) {
    pallas::ErrorReporter::addError(SOURCE_LOC,
                                    "Malformed specification: " + msg, &md);
}

void addArgFromGhostVar(const llvm::MDNode &gVarDef,
                        col::LlvmFunctionInvocation &colWrapperCall,
                        const pallas::irspec::WrappedSpecElement &specElem,
                        llvm::Function &pFunc,
                        llvm::FunctionAnalysisManager &fam) {
    auto &parentCRes = fam.getResult<pallas::FunctionContractDeclarer>(pFunc);
    auto *gVar = parentCRes.getGhostArgMapEntry(gVarDef);
    if (gVar == nullptr) {
        printError(gVarDef, "Unable to find COL-variable for ghost-argument");
        return;
    }
    // TODO: Perhaps this needs to be a deref if the ghost-var is a
    // struct?!
    auto *argExpr = colWrapperCall.add_args()->mutable_local();
    argExpr->set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(specElem));
    argExpr->mutable_ref()->set_id(gVar->id());
}

/**
 * Initialize called function of origin of the given wrapper-call
 */
void initWrapperCallBase(col::LlvmFunctionInvocation &colWrapperCall,
                         const pallas::irspec::WrappedSpecElement &specElem,
                         llvm::FunctionAnalysisManager &fam) {
    auto &colWrapper =
        fam.getResult<pallas::FunctionDeclarer>(specElem.getWrapper())
            .getAssociatedColFuncDef();

    // Initialize call
    colWrapperCall.set_allocated_origin(
        llvm2col::generatePallasWrapperCallOrigin(specElem));
    colWrapperCall.set_allocated_blame(new col::Blame());
    colWrapperCall.mutable_ref()->set_id(colWrapper.id());
}

} // namespace

void buildWrapperCall(const pallas::irspec::WrappedSpecElement &specElem,
                      llvm::Value &matchedValue, llvm::Function &pFunc,
                      col::LlvmFunctionInvocation &colWrapperCall,
                      pallas::FunctionCursor &functionCursor,
                      varToIntrMapping diVarMapper) {
    auto &fam = functionCursor.getFunctionAnalysisManager();

    // Init called function & origin
    initWrapperCallBase(colWrapperCall, specElem, fam);

    // Map args of wrapper to DIVars
    auto wArgMapping = pallas::utils::mapArgsToDIVars(specElem.getWrapper());
    if (!wArgMapping.has_value()) {
        printError(specElem.getWrapper(),
                   "Unable to map args of wrapper to DIVariables");
        return;
    }

    // Build call-args
    for (const auto &wArg : specElem.getWrapper().args()) {
        // Get DIVar for wrapper-arg
        if (!wArgMapping->contains(&wArg)) {
            printError(specElem.getWrapper(),
                       "Unable to map argument to DIVariable");
            return;
        }
        auto *wDiVar = wArgMapping->at(&wArg);

        // Find corresponding source from spec-encoding:
        if (auto *pDiVar = specElem.getParentVar(wDiVar)) {
            // Build arg from regular variable
            bool argOk = buildArgForDIVar(*pDiVar, matchedValue, specElem,
                                          colWrapperCall, wArg.getArgNo(),
                                          functionCursor, diVarMapper);
            if (!argOk)
                return;
        } else if (auto *gDef = specElem.getGhostDef(wDiVar)) {
            // Build arg from definition of ghost variable
            addArgFromGhostVar(*gDef, colWrapperCall, specElem, pFunc, fam);
        } else {
            // Error, no mapping for DIVaraible
            std::stringstream s;
            s << "Failed to build argument for wrapper call to '"
              << specElem.getWrapper().getName().str() << "'."
              << "No mapping for argument '" << wArg.getName().str() << "'";
            printError(pFunc, s.str());
            return;
        }
    }
}

void buildContractWrapperCall(const pallas::irspec::ContractClause &clause,
                              llvm::Function &pFunc,
                              col::LlvmFunctionInvocation &colWrapperCall,
                              llvm::FunctionAnalysisManager &fam,
                              bool isExternal) {
    if (isExternal) {
        buildExternalWrapperCall(clause, pFunc, colWrapperCall, fam);
        return;
    }

    // Init called function & origin
    initWrapperCallBase(colWrapperCall, clause, fam);

    // Map args of wrapper to DIVars
    auto wArgMapping = pallas::utils::mapArgsToDIVars(clause.getWrapper());
    if (!wArgMapping.has_value()) {
        printError(clause.getWrapper(),
                   "Unable to map args of wrapper to DIVariables");
        return;
    }

    // Build call-args
    for (const auto &wArg : clause.getWrapper().args()) {
        // Get DIVar for wrapper-arg
        if (!wArgMapping->contains(&wArg)) {
            printError(clause.getWrapper(),
                       "Unable to map argument to DIVariable");
            return;
        }
        auto *wDiVar = wArgMapping->at(&wArg);

        if (auto *pDiVar = clause.getParentVar(wDiVar)) {
            // Build arg from regular variable
            auto pArg = utils::mapDIVarToArg(pFunc, *pDiVar);
            if (pArg == nullptr) {
                printError(pFunc, "Unable to map DIVariable to argument.");
                return;
            }
            auto &colArg = fam.getResult<pallas::FunctionDeclarer>(pFunc)
                               .getFuncArgMapEntry(*pArg);
            // Construct Local-node that references the variable
            auto *argExpr = colWrapperCall.add_args()->mutable_local();
            argExpr->set_allocated_origin(
                llvm2col::generatePallasWrapperCallOrigin(clause));
            argExpr->mutable_ref()->set_id(colArg.id());
        } else if (auto *gDef = clause.getGhostDef(wDiVar)) {
            // Build arg from definition of ghost variable
            addArgFromGhostVar(*gDef, colWrapperCall, clause, pFunc, fam);
        } else {
            // Error, no mapping for DIVaraible
            std::stringstream s;
            s << "Failed to build argument for wrapper call to '"
              << clause.getWrapper().getName().str() << "'."
              << "No mapping for argument '" << wArg.getName().str() << "'";
            printError(pFunc, s.str());
            return;
        }
    }
}

void buildExternalWrapperCall(
    const pallas::irspec::WrappedSpecElement &specElem, llvm::Function &pFunc,
    col::LlvmFunctionInvocation &colWrapperCall,
    llvm::FunctionAnalysisManager &fam) {

    if (specElem.getWrapper().arg_size() != pFunc.arg_size()) {
        printError(
            specElem.getWrapper(),
            "The number of argument does not match between the parent function "
            "and the wrapper function of the external contract.");
        return;
    }

    // Init called function & origin
    initWrapperCallBase(colWrapperCall, specElem, fam);

    // Assume that wrapper-args are the same as those of the external function
    for (auto [wArg, pArg] :
         llvm::zip_equal(specElem.getWrapper().args(), pFunc.args())) {

        if (wArg.getType() != pArg.getType()) {
            printError(
                specElem.getWrapper(),
                "The signature does not match between the parent function "
                "and the wrapper function of the external contract.");
            return;
        }
        auto &colVar =
            fam.getResult<pallas::FunctionDeclarer>(pFunc).getFuncArgMapEntry(
                pArg);

        // Construct Local-node that references the variable and add it to
        // the list of arguments
        auto *argExpr = colWrapperCall.add_args()->mutable_local();
        // TODO: Currently this just points to the full clause.
        //       Could be extended to point to the specific variable instead.
        argExpr->set_allocated_origin(
            llvm2col::generatePallasWrapperCallOrigin(specElem));
        auto *varRef = argExpr->mutable_ref();
        varRef->set_id(colVar.id());
    }

    // TODO: Implement for given- and yields arguments!
    if (specElem.getNumGhostArgs() > 0) {
        printError(specElem.getWrapper(), "Ghost arguments are currently not "
                                          "supported for external contracts!");
        return;
    }
}

bool buildArgForDIVar(llvm::DIVariable &diVar, llvm::Value &matchedValue,
                      const pallas::irspec::WrappedSpecElement &specElem,
                      col::LlvmFunctionInvocation &wrapperCall,
                      unsigned int argIdx,
                      pallas::FunctionCursor &functionCursor,
                      varToIntrMapping diVarMapper) {
    auto *diLocVar = llvm::dyn_cast<llvm::DILocalVariable>(&diVar);
    if (diLocVar == nullptr) {
        printError(diVar, "Global DIVariables are currently unsupported");
        return false;
    }

    auto *intr = diVarMapper(*diLocVar, matchedValue,
                             functionCursor.getFunctionAnalysisManager());
    if (intr == nullptr) {
        printError(diVar, "Unable to map DIVariable to intrinsic.");
        return false;
    }

    if (auto *dbgDecl = llvm::dyn_cast<llvm::DbgDeclareInst>(intr)) {
        // Mapped to dbgDeclare
        auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(dbgDecl->getAddress());
        if (alloca == nullptr) {
            printError(*dbgDecl, "Currently, only alloca is supported "
                                 "as a target for dbg.declare.");
            return false;
        }
        utils::buildArgExprFromAlloca(wrapperCall, specElem, argIdx, *alloca,
                                      functionCursor);
    } else if (auto *dbgValue = llvm::dyn_cast<llvm::DbgValueInst>(intr)) {
        // Mapped to DbgValue
        bool ok = utils::buildArgExprFromDbgValue(wrapperCall, specElem, argIdx,
                                                  *dbgValue, functionCursor);
        if (!ok) {
            printError(*dbgValue,
                       "Unable to build argument of wrapper-function");
            return false;
        }
    } else {
        printError(*intr, "Unable to build arg from dbg-intrinsic.");
        return false;
    }

    return true;
}

} // namespace llvm2col