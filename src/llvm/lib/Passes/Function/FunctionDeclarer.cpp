#include "Passes/Function/FunctionDeclarer.h"
#include "Passes/Function/ExprWrapperMapper.h"

#include "Origin/OriginProvider.h"
#include "Passes/Module/RootContainer.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Function::FunctionDeclarer";
using namespace llvm;

/**
 * Checks function definition for unsupported features that might change
 * semantics and adds warning if this is the case.
 * @param llvmFunction: the function to be checked
 */
void checkFunctionSupport(llvm::Function &llvmFunction) {
    // TODO add syntax support checks that change the semantics of the program
    // to function definitions
    // TODO see: https://releases.llvm.org/15.0.0/docs/LangRef.html#functions
}

/*
 * Function Declarer Result
 */

FDResult::FDResult(col::LlvmFunctionDefinition &colFuncDef,
                   ColScopedFuncBody associatedScopedColFuncBody,
                   int64_t functionId)
    : associatedColFuncDef(colFuncDef),
      associatedScopedColFuncBody(associatedScopedColFuncBody),
      functionId(functionId) {}

col::LlvmFunctionDefinition &FDResult::getAssociatedColFuncDef() {
    return associatedColFuncDef;
}

ColScopedFuncBody FDResult::getAssociatedScopedColFuncBody() {
    return associatedScopedColFuncBody;
}

void FDResult::addFuncArgMapEntry(Argument &llvmArg, col::Variable &colArg) {
    funcArgMap.insert({&llvmArg, &colArg});
}

col::Variable &FDResult::getFuncArgMapEntry(Argument &arg) {
    return *funcArgMap.at(&arg);
}

int64_t &FDResult::getFunctionId() { return functionId; }

/*
 * Function Declarer (Analysis)
 */
AnalysisKey FunctionDeclarer::Key;

FDResult FunctionDeclarer::run(Function &F, FunctionAnalysisManager &FAM) {
    auto MAM = FAM.getResult<ModuleAnalysisManagerFunctionProxy>(F);
    auto pProgram = MAM.getCachedResult<RootContainer>(*F.getParent())->program;
    checkFunctionSupport(F);
    // create llvmFuncDef declaration in buffer
    col::GlobalDeclaration *llvmFuncDefDecl = pProgram->add_declarations();
    // generate id
    col::LlvmFunctionDefinition *llvmFuncDef =
        llvmFuncDefDecl->mutable_llvm_function_definition();
    int64_t functionId = llvm2col::setColNodeId(llvmFuncDef);
    // add body block + scope + origin
    llvmFuncDef->set_allocated_blame(new col::Blame());
    // set origin
    llvmFuncDef->set_allocated_origin(llvm2col::generateFuncDefOrigin(F));
    ColScopedFuncBody funcScopedBody{};
    if (!F.isDeclaration()) {
        funcScopedBody.scope =
            llvmFuncDef->mutable_function_body()->mutable_scope();
        funcScopedBody.scope->set_allocated_origin(
            llvm2col::generateFuncDefOrigin(F));
        funcScopedBody.block =
            funcScopedBody.scope->mutable_body()->mutable_block();
        funcScopedBody.block->set_allocated_origin(
            llvm2col::generateFuncDefOrigin(F));
    }
    FDResult result = FDResult(*llvmFuncDef, funcScopedBody, functionId);
    // set args (if present)
    for (llvm::Argument &llvmArg : F.args()) {
        // set in buffer
        col::Variable *colArg = llvmFuncDef->add_args();
        // set origin
        colArg->set_allocated_origin(llvm2col::generateArgumentOrigin(llvmArg));
        llvm2col::setColNodeId(colArg);
        try {
            llvm2col::transformAndSetType(*llvmArg.getType(),
                                          *colArg->mutable_t());
        } catch (pallas::UnsupportedTypeException &e) {
            std::stringstream errorStream;
            errorStream << e.what() << " in argument #" << llvmArg.getArgNo();
            pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
        }
        // add args mapping to result
        result.addFuncArgMapEntry(llvmArg, *colArg);
    }
    llvmFuncDef->set_allocated_blame(new col::Blame());
    // complete the function declaration in proto buffer
    // set return type in protobuf of function
    try {
        llvm2col::transformAndSetType(*F.getReturnType(),
                                      *llvmFuncDef->mutable_return_type());
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << e.what() << " in return signature";
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
    }

    if (utils::isPallasExprWrapper(F)) {
        auto mapperResult = FAM.getResult<pallas::ExprWrapperMapper>(F);
        auto *wrapperParent = mapperResult.getParentFunc();
        if (wrapperParent != nullptr) {
            auto colParent = FAM.getResult<FunctionDeclarer>(*wrapperParent);
            llvmFuncDef->mutable_pallas_expr_wrapper_for()->set_id(
                colParent.getFunctionId());
        } else {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Wrapper-function without parent!", F);
        }
    }

    try {
        if (F.getParamStructRetType(0) != nullptr) {
            auto retIdxT = llvmFuncDef->mutable_return_in_param();
            retIdxT->set_v1(0);
            llvm2col::transformAndSetPointerType(*F.getParamStructRetType(0),
                                                 *retIdxT->mutable_v2());
        } else if (F.getParamStructRetType(1) != nullptr) {
            auto retIdxT = llvmFuncDef->mutable_return_in_param();
            retIdxT->set_v1(1);
            llvm2col::transformAndSetPointerType(*F.getParamStructRetType(1),
                                                 *retIdxT->mutable_v2());
        }
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << e.what() << " in sret argument ";
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
    }

    if (F.isDeclaration()) {
        // Defined outside of this module so we don't know if it's pure or what
        // its contract is
        col::VcllvmFunctionContract *colContract =
            llvmFuncDef->mutable_contract()->mutable_vcllvm_function_contract();
        colContract->set_allocated_blame(new col::Blame());
        colContract->set_value("requires true;");
        colContract->set_name(F.getName());
        colContract->set_allocated_origin(new col::Origin());

        llvmFuncDef->set_pure(false);
    }
    return result;
}

/*
 * Function Declarer Pass
 */
PreservedAnalyses FunctionDeclarerPass::run(Function &F,
                                            FunctionAnalysisManager &FAM) {

    // TODO: Check if the function is part of the spec-lib library.
    // If so, skip it.

    FDResult result = FAM.getResult<FunctionDeclarer>(F);
    // Just makes sure we analyse every function
    return PreservedAnalyses::all();
}
} // namespace pallas
