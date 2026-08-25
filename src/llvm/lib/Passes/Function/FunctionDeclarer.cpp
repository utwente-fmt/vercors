#include "Passes/Function/FunctionDeclarer.h"
#include "IRSpec/PallasSpecDecoding.h"
#include "Passes/Function/ExprWrapperMapper.h"

#include "Origin/OriginProvider.h"
#include "Passes/Module/RootContainer.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "Util/PallasMD.h"
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

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
    llvm::DITypeRefArray typeArray = nullptr;
    if (const auto *subProgram = F.getSubprogram()) {
        if (const auto *subProgramType =
                llvm::dyn_cast_or_null<DISubroutineType>(
                    subProgram->getType())) {
            typeArray = subProgramType->getTypeArray();
        }
    }
    auto *sdRes = MAM.getCachedResult<StructTDeclarer>(*F.getParent());
    assert(sdRes != nullptr);
    // set args (if present)
    for (llvm::Argument &llvmArg : F.args()) {

        // Argument
        auto *arg = llvmFuncDef->add_llvm_args();
        arg->set_allocated_origin(llvm2col::generateArgumentOrigin(llvmArg));

        // Add 'byval'-attribute
        if (llvmArg.hasByValAttr()) {
            auto *colByVal = arg->add_attributes()->mutable_llvm_by_val_arg();
            colByVal->set_allocated_origin(
                llvm2col::generateArgumentOrigin(llvmArg));
            llvm2col::transformAndSetType(*llvmArg.getParamByValType(),
                                          *colByVal->mutable_t(), *sdRes);
        }
        // Add sret attribute
        if (llvmArg.hasStructRetAttr()) {
            auto *colSret = arg->add_attributes()->mutable_llvm_sret_arg();
            colSret->set_allocated_origin(
                llvm2col::generateArgumentOrigin(llvmArg));
            llvm2col::transformAndSetType(*llvmArg.getParamStructRetType(),
                                          *colSret->mutable_t(), *sdRes);
        }

        // Variable
        // set in buffer
        auto *colVar = arg->mutable_v();
        // set origin
        colVar->set_allocated_origin(llvm2col::generateArgumentOrigin(llvmArg));
        llvm2col::setColNodeId(colVar);
        llvm::Type *pointerType = llvmArg.getParamStructRetType();
        if (pointerType == nullptr)
            pointerType = llvmArg.getParamByRefType();
        if (pointerType == nullptr)
            pointerType = llvmArg.getParamByValType();
        if (pointerType == nullptr)
            pointerType = llvmArg.getParamInAllocaType();
        if (pointerType == nullptr &&
            llvmArg.hasAttribute(llvm::Attribute::ElementType))
            pointerType = llvmArg.getAttribute(llvm::Attribute::ElementType)
                              .getValueAsType();
        try {
            llvm2col::transformAndSetValueType(llvmArg, pointerType,
                                               *colVar->mutable_t(), *sdRes);
        } catch (pallas::UnsupportedTypeException &e) {
            std::stringstream errorStream;
            errorStream << e.what() << " in argument #" << llvmArg.getArgNo();
            pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
        }
        // add args mapping to result
        result.addFuncArgMapEntry(llvmArg, *colVar);
    }
    llvmFuncDef->set_allocated_blame(new col::Blame());
    // complete the function declaration in proto buffer
    // set return type in protobuf of function
    try {
        if (typeArray.size() > 0) {
            llvm2col::transformAndSetTypeWithDebugInfo(
                F.getReturnType(), typeArray[0],
                *llvmFuncDef->mutable_return_type(), *sdRes);
        } else {
            llvm2col::transformAndSetType(*F.getReturnType(),
                                          *llvmFuncDef->mutable_return_type(),
                                          *sdRes);
        }
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << " in return signature";
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
    }

    // Set type-flag of the function
    int numTypeDefs = 0;
    numTypeDefs += irspec::isPallasExprWrapper(F) ? 1 : 0;
    numTypeDefs += irspec::isPallasGhostWrapper(F) ? 1 : 0;
    numTypeDefs += irspec::isPallasPredDef(F) ? 1 : 0;

    auto *fType = llvmFuncDef->mutable_function_type();
    if (numTypeDefs > 1) {
        std::stringstream errorStream;
        errorStream << "Functions may not be marked as both"
                    << " a wrapper AND a predicate definition!";
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
    } else if (irspec::isPallasExprWrapper(F)) {
        fType->mutable_wrapper_function()->set_allocated_origin(
            llvm2col::generateFuncDefOrigin(F));
    } else if (irspec::isPallasGhostWrapper(F)) {
        fType->mutable_ghost_wrapper_function()->set_allocated_origin(
            llvm2col::generateFuncDefOrigin(F));
    } else if (irspec::isPallasPredDef(F)) {
        auto isInline = irspec::isPallasPredInline(F);
        if (isInline.has_value()) {
            auto *predTy = fType->mutable_predicate_definition();
            predTy->set_allocated_origin(llvm2col::generateFuncDefOrigin(F));
            predTy->set_inlined(*isInline);
        } else {
            pallas::ErrorReporter::addError(SOURCE_LOC,
                                            "Invalid predicate definition!", F);
        }
    } else {
        fType->mutable_normal_function()->set_allocated_origin(
            llvm2col::generateFuncDefOrigin(F));
    }

    if (irspec::isPallasExprWrapper(F) || irspec::isPallasGhostWrapper(F)) {
        auto mapperResult = FAM.getResult<pallas::ExprWrapperMapper>(F);
        auto *wrapperParent = mapperResult.getParentFunc();
        if (wrapperParent == nullptr) {
            pallas::ErrorReporter::addError(
                SOURCE_LOC, "Wrapper-function without parent!", F);
        }
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

    // Function Attributes
    llvmFuncDef->set_has_noreturn_attr(
        F.hasFnAttribute(llvm::Attribute::NoReturn));

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
