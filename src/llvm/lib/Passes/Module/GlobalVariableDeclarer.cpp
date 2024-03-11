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

            llvm2col::transformAndSetType(*global.getInitializer()->getType(),
                                          *colGlobal->mutable_variable_type());
        } else {
            // We don't know more about the type because we don't have an
            // initializer
            // TODO: This breaks the assumption that the type of the global
            // declaration type is the inner type of the pointer. We should
            // instead set the type to be TAny maybe?
            llvm2col::transformAndSetType(*global.getType(),
                                          *colGlobal->mutable_variable_type());
        }
        colGlobal->set_constant(global.isConstant());
        colGlobal->set_allocated_origin(
            llvm2col::generateGlobalVariableOrigin(M, global));
        colGlobal->set_id(reinterpret_cast<int64_t>(&global));
    }

    return PreservedAnalyses::all();
}

} // namespace pallas
