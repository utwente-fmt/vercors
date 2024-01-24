#include <sstream>
#include "Passes/Function/FunctionContractDeclarer.h"

#include "Passes/Function/FunctionDeclarer.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Origin/OriginProvider.h"


namespace vcllvm {
    const std::string SOURCE_LOC = "Passes::Function::FunctionContractDeclarer";

    using namespace llvm;

    /*
     * Function Contract Declarer Result
     */

    FDCResult::FDCResult(vct::col::serialize::LlvmFunctionContract &colFuncContract) :
            associatedColFuncContract(colFuncContract) {}

    col::LlvmFunctionContract &FDCResult::getAssociatedColFuncContract() {
        return associatedColFuncContract;
    }

    /*
     * Function Contract Declarer (Analysis)
     */

    AnalysisKey FunctionContractDeclarer::Key;


    FunctionContractDeclarer::FunctionContractDeclarer(std::shared_ptr<col::Program> pProgram) :
            pProgram(std::move(pProgram)) {}

    FunctionContractDeclarer::Result FunctionContractDeclarer::run(Function &F, FunctionAnalysisManager &FAM) {
        // fetch relevant function from the Function Declarer
        FDResult fdResult = FAM.getResult<FunctionDeclarer>(F);
        col::LlvmFunctionDefinition &colFunction = fdResult.getAssociatedColFuncDef();
        // set a contract in the buffer as well as make and return a result object
        return FDCResult(*colFunction.mutable_contract());
    }

    /*
     * Function Contract Declarer Pass
     */

    FunctionContractDeclarerPass::FunctionContractDeclarerPass(std::shared_ptr<col::Program> pProgram) :
            pProgram(std::move(pProgram)) {}

    PreservedAnalyses FunctionContractDeclarerPass::run(Function &F, FunctionAnalysisManager &FAM) {
        // get col contract
        FDCResult result = FAM.getResult<FunctionContractDeclarer>(F);
        col::LlvmFunctionContract &colContract = result.getAssociatedColFuncContract();
        // check if contract keyword is present
        if (!F.hasMetadata(vcllvm::constants::METADATA_CONTRACT_KEYWORD)) {
            // set contract to a tautology
            colContract.set_value("requires true;");
            colContract.set_origin("{}");
            return PreservedAnalyses::all();
        }
        // concatenate all contract lines with new lines
        MDNode *contractMDNode = F.getMetadata(vcllvm::constants::METADATA_CONTRACT_KEYWORD);
        std::stringstream contractStream;
        for (u_int32_t i = 0; i < contractMDNode->getNumOperands(); i++) {
            auto contractLine = dyn_cast<MDString>(contractMDNode->getOperand(i));
            if (contractLine == nullptr) {
                std::stringstream errorStream;
                errorStream << "Unable to cast contract metadata node #" << i + 1 << "to string type";
                vcllvm::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), F);
                break;
            }
            contractStream << contractLine->getString().str() << '\n';
        }
        colContract.set_value(contractStream.str());
        colContract.set_origin(llvm2Col::generateFunctionContractOrigin(F, contractStream.str()));
        // add all callable functions to the contracts invokables
        for(auto &moduleF : F.getParent()->functions()) {
            std::string fName = '@' + moduleF.getName().str();
            int64_t fId = FAM.getResult<FunctionDeclarer>(moduleF).getFunctionId();
            col::StringRef *invokeRef = colContract.add_invokable_refs();
            invokeRef->set_v1(fName);
            invokeRef->mutable_v2()->set_index(fId);
        }
        return PreservedAnalyses::all();
    }
}