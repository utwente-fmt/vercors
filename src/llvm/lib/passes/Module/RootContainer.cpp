#include "Passes/Module/RootContainer.h"

#include "Transform/Transform.h"
#include "Origin/OriginProvider.h"
#include "Util/Exceptions.h"

namespace vcllvm {
    const std::string SOURCE_LOC = "Passes::Module::RootContainer";
    using namespace llvm;

    bool ProgramWrapper::invalidate(Module &M, const PreservedAnalyses &PA, ModuleAnalysisManager::Invalidator &) {
        return !PA.getChecker<RootContainer>().preservedWhenStateless();
    }

    AnalysisKey RootContainer::Key;

    ProgramWrapper RootContainer::run(Module &M, ModuleAnalysisManager &MAM) {
        auto pProgram = std::make_shared<col::Program>();
        // set program origin
        pProgram->set_allocated_origin(llvm2Col::generateProgramOrigin(M));
        pProgram->set_allocated_blame(new col::Blame());

        return ProgramWrapper { pProgram };
    }
}
