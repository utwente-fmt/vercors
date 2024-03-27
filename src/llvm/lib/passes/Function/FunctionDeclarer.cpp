#include <sstream>
#include "Passes/Function/FunctionDeclarer.h"

#include "Transform/Transform.h"
#include "Origin/OriginProvider.h"
#include "Util/Exceptions.h"


namespace vcllvm {
    const std::string SOURCE_LOC = "Passes::Function::FunctionDeclarer";
    using namespace llvm;

    /**
     * Checks function definition for unsupported features that might change semantics and
     * adds warning if this is the case.
     * @param llvmFunction: the function to be checked
     */
    void checkFunctionSupport(llvm::Function &llvmFunction) {
        // TODO add syntax support checks that change the semantics of the program to function definitions
        // TODO see: https://releases.llvm.org/15.0.0/docs/LangRef.html#functions
    }

    /*
     * Function Declarer Result
     */

    FDResult::FDResult(col::LlvmFunctionDefinition &colFuncDef,
                       ColScopedFuncBody associatedScopedColFuncBody,
                       int64_t functionId) :
            associatedColFuncDef(colFuncDef),
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

    int64_t &FDResult::getFunctionId() {
        return functionId;
    }


    /*
     * Function Declarer (Analysis)
     */
    AnalysisKey FunctionDeclarer::Key;

    FunctionDeclarer::FunctionDeclarer(std::shared_ptr<col::Program> pProgram) :
            pProgram(std::move(pProgram)) {}

    FDResult FunctionDeclarer::run(Function &F, FunctionAnalysisManager &FAM) {
        checkFunctionSupport(F);
        // create llvmFuncDef declaration in buffer
        col::GlobalDeclaration *llvmFuncDefDecl = pProgram->add_declarations();
        // generate id
        col::LlvmFunctionDefinition *llvmFuncDef = llvmFuncDefDecl->mutable_llvm_function_definition();
        int64_t functionId = llvm2Col::setColNodeId(llvmFuncDef);
        // add body block + scope + origin
        // set origin
        llvmFuncDef->set_allocated_origin(llvm2Col::generateFuncDefOrigin(F));
        llvmFuncDef->set_allocated_blame(new col::Blame());
        ColScopedFuncBody funcScopedBody{};
        funcScopedBody.scope = llvmFuncDef->mutable_function_body()->mutable_scope();
        funcScopedBody.scope->set_allocated_origin(llvm2Col::generateFuncDefOrigin(F));
        funcScopedBody.block = funcScopedBody.scope->mutable_body()->mutable_block();
        funcScopedBody.block->set_allocated_origin(llvm2Col::generateFuncDefOrigin(F));
        FDResult result = FDResult(*llvmFuncDef, funcScopedBody, functionId);
        // set args (if present)
        for (llvm::Argument &llvmArg: F.args()) {
            // set in buffer
            col::Variable *colArg = llvmFuncDef->add_args();
            // set origin
            colArg->set_allocated_origin(llvm2Col::generateArgumentOrigin(llvmArg));
            llvm2Col::setColNodeId(colArg);
            try {
                llvm2Col::transformAndSetType(*llvmArg.getType(), *colArg->mutable_t());
            } catch (vcllvm::UnsupportedTypeException &e) {
                std::stringstream errorStream;
                errorStream << e.what() << " in argument #" << llvmArg.getArgNo();
                vcllvm::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
            }
            // add args mapping to result
            result.addFuncArgMapEntry(llvmArg, *colArg);
        }
        return result;
    }

    /*
     * Function Declarer Pass
     */
    FunctionDeclarerPass::FunctionDeclarerPass(std::shared_ptr<col::Program> pProgram) :
            pProgram(std::move(pProgram)) {}

    PreservedAnalyses FunctionDeclarerPass::run(Function &F, FunctionAnalysisManager &FAM) {
        FDResult result = FAM.getResult<FunctionDeclarer>(F);
        col::LlvmFunctionDefinition &colFunction = result.getAssociatedColFuncDef();
        // complete the function declaration in proto buffer
        // set return type in protobuf of function
        try {
            llvm2Col::transformAndSetType(*F.getReturnType(), *colFunction.mutable_return_type());
        } catch (vcllvm::UnsupportedTypeException &e) {
            std::stringstream errorStream;
            errorStream << e.what() << " in return signature";
            vcllvm::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
        }
        return PreservedAnalyses::all();
    }
}
