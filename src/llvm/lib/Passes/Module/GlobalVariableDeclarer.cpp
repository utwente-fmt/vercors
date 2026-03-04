#include "Passes/Module/GlobalVariableDeclarer.h"
#include "Passes/Module/RootContainer.h"
#include "Passes/Module/StructTDeclarer.h"
#include "Transform/Transform.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfoMetadata.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::GlobalVariableDeclarer";

using namespace llvm;

PreservedAnalyses GlobalVariableDeclarerPass::run(Module &M,
                                                  ModuleAnalysisManager &MAM) {
    auto pProgram = MAM.getResult<RootContainer>(M).program;
    auto &sdResult = MAM.getResult<StructTDeclarer>(M);

    for (auto &global : M.globals()) {
        llvm::SmallVector<llvm::DIGlobalVariableExpression *> debugExprs;
        global.getDebugInfo(debugExprs);

        DIType *diType = nullptr;
        for (auto *debugExpr : debugExprs) {
            DIType *t = debugExpr->getVariable()->getType();
            if (diType != nullptr && diType != t) {
                ErrorReporter::addError(
                    SOURCE_LOC, "Found conflicting DITypes for global variable",
                    global.getName().str());
                diType = nullptr;
                break;
            }
            diType = t;
        }

        col::GlobalDeclaration *globDecl = pProgram->add_declarations();
        col::LlvmGlobalVariable *colGlobal =
            globDecl->mutable_llvm_global_variable();

        if (diType == nullptr) {
            if (global.hasInitializer()) {
                // Skip the entry-point global from swift, as it contains
                // currently unsupported pointer-casting and is not needed for
                // verification.
                if (global.getSection().str() !=
                    constants::SWIFT_ENTRY_SECTION) {
                    llvm2col::transformAndSetConstExpr(
                        MAM.getResult<FunctionAnalysisManagerModuleProxy>(M)
                            .getManager(),
                        llvm2col::generateGlobalVariableInitializerOrigin(
                            M, global, *global.getInitializer()),
                        *global.getInitializer(), *colGlobal->mutable_value(),
                        sdResult);
                }
                // TODO: What to do here? How can we get the DIType?
                llvm2col::transformAndSetType(
                    *global.getInitializer()->getType(),
                    *colGlobal->mutable_variable_type(), sdResult);
            } else {
                // We don't know more about the type because we don't have an
                // initializer
                // TODO: This breaks the assumption that the type of the global
                // declaration type is the inner type of the pointer. We should
                // instead set the type to be TAny maybe?
                llvm2col::transformAndSetType(
                    *global.getType(), *colGlobal->mutable_variable_type(),
                    sdResult);
            }
        } else {
            llvm2col::transformAndSetTypeWithDebugInfo(
                global.getType(), diType, *colGlobal->mutable_variable_type(),
                sdResult);
        }

        colGlobal->set_constant(global.isConstant());
        colGlobal->set_allocated_origin(
            llvm2col::generateGlobalVariableOrigin(M, global));
        colGlobal->set_id(reinterpret_cast<int64_t>(&global));
    }

    return PreservedAnalyses::all();
}

} // namespace pallas
