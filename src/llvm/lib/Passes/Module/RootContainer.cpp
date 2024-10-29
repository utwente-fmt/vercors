#include "Passes/Module/RootContainer.h"

#include "Origin/OriginProvider.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::RootContainer";
using namespace llvm;

bool ProgramWrapper::invalidate(Module &M, const PreservedAnalyses &PA,
                                ModuleAnalysisManager::Invalidator &) {
    return !PA.getChecker<RootContainer>().preservedWhenStateless();
}

AnalysisKey RootContainer::Key;

ProgramWrapper RootContainer::run(Module &M, ModuleAnalysisManager &MAM) {
    auto pProgram = std::make_shared<col::Program>();
    // set program origin
    pProgram->set_allocated_origin(llvm2col::generateProgramOrigin(M));
    pProgram->set_allocated_blame(new col::Blame());

    return ProgramWrapper{pProgram};
}
} // namespace pallas
