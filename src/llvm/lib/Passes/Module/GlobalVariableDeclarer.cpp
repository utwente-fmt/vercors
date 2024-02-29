#include "Passes/Module/GlobalVariableDeclarer.h"
#include "Passes/Module/RootContainer.h"
#include "Transform/Transform.h"

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::GlobalVariableDeclarer";

using namespace llvm;

PreservedAnalyses GlobalVariableDeclarerPass::run(Module &M,
                                                  ModuleAnalysisManager &MAM) {
    auto pProgram = MAM.getResult<RootContainer>(M).program;

    for (auto &global : M.globals()) {
        col::GlobalDeclaration *globDecl = pProgram->add_declarations();
        col::LlvmGlobalVariable *colGlobal =
            globDecl->mutable_llvm_global_variable();

        llvm2col::transformAndSetType(*global.getType(),
                                      *colGlobal->mutable_variable_type());
        if (global.hasInitializer()) {
            llvm2col::transformAndSetConstExpr(
                MAM.getResult<FunctionAnalysisManagerModuleProxy>(M)
                    .getManager(),
                llvm2col::generateGlobalVariableInitializerOrigin(
                    M, global, *global.getInitializer()),
                *global.getInitializer(), *colGlobal->mutable_value());
        }
        colGlobal->set_constant(global.isConstant());
        colGlobal->set_allocated_origin(
            llvm2col::generateGlobalVariableOrigin(M, global));
        llvm2col::setColNodeId(colGlobal);
    }

    return PreservedAnalyses::all();
}

} // namespace pallas
