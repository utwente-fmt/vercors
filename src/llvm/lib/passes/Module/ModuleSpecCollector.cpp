#include "Passes/Module/ModuleSpecCollector.h"
#include "Passes/Module/RootContainer.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Origin/OriginProvider.h"
#include "Transform/Transform.h"

namespace vcllvm {
    const std::string SOURCE_LOC = "Passes::Module::ModuleSpecCollector";

    using namespace llvm;

    PreservedAnalyses ModuleSpecCollectorPass::run(Module &M, ModuleAnalysisManager &MAM) {
        auto pProgram = MAM.getResult<RootContainer>(M).program;
        NamedMDNode *globalMDNode = M.getNamedMetadata(vcllvm::constants::METADATA_GLOBAL_KEYWORD);
        if(globalMDNode == nullptr) {
            return PreservedAnalyses::all();
        }
        for (u_int32_t i = 0; i < globalMDNode->getNumOperands(); i++) {
            for (u_int32_t j = 0; j < globalMDNode->getOperand(i)->getNumOperands(); j++) {
                auto globVal = dyn_cast<MDString>(globalMDNode->getOperand(i)->getOperand(j));
                if (globVal == nullptr) {
                    std::stringstream errorStream;
                    errorStream << "Unable to cast global metadata node #" << i + 1 << "to string type";
                    vcllvm::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), M);
                    break;
                }
                col::GlobalDeclaration *globDecl = pProgram->add_declarations();
                col::LlvmGlobal *colGlobal = globDecl->mutable_llvm_global();
                llvm2Col::setColNodeId(colGlobal);
                colGlobal->set_value(globVal->getString().str());
                colGlobal->set_allocated_origin(llvm2Col::generateGlobalValOrigin(M, globVal->getString().str()));
            }
        }
        return PreservedAnalyses::all();
    }

}
