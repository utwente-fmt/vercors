#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include "Passes/Function/FunctionBodyTransformer.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Passes/Function/PureAssigner.h"
#include "Passes/Module/GlobalVariableDeclarer.h"
#include "Passes/Module/ModuleSpecCollector.h"
#include "Passes/Module/ProtobufPrinter.h"
#include "Passes/Module/RootContainer.h"

using namespace llvm;

llvm::PassPluginLibraryInfo getPallasPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "Pallas", LLVM_VERSION_STRING,
            [](PassBuilder &PB) {
                PB.registerAnalysisRegistrationCallback(
                    [](llvm::ModuleAnalysisManager &MAM) {
                        MAM.registerPass(
                            [&] { return pallas::RootContainer(); });
                    });
                PB.registerAnalysisRegistrationCallback(
                    [](llvm::FunctionAnalysisManager &FAM) {
                        FAM.registerPass(
                            [&] { return pallas::FunctionDeclarer(); });
                        FAM.registerPass(
                            [&] { return pallas::FunctionContractDeclarer(); });
                    });
                PB.registerPipelineParsingCallback(
                    [](StringRef Name, llvm::ModulePassManager &MPM,
                       ArrayRef<llvm::PassBuilder::PipelineElement>) {
                        if (Name == "pallas-collect-module-spec") {
                            MPM.addPass(pallas::ModuleSpecCollectorPass());
                            return true;
                        } else if (Name == "pallas-declare-variables") {
                            MPM.addPass(pallas::GlobalVariableDeclarerPass());
                            return true;
                        } else if (Name == "pallas-print-protobuf") {
                            MPM.addPass(pallas::ProtobufPrinter());
                            return true;
                        }
                        return false;
                    });
                PB.registerPipelineParsingCallback(
                    [](StringRef Name, llvm::FunctionPassManager &FPM,
                       ArrayRef<llvm::PassBuilder::PipelineElement>) {
                        if (Name == "pallas-declare-function") {
                            FPM.addPass(pallas::FunctionDeclarerPass());
                            return true;
                        } else if (Name == "pallas-assign-pure") {
                            FPM.addPass(pallas::PureAssignerPass());
                            return true;
                        } else if (Name == "pallas-declare-function-contract") {
                            FPM.addPass(pallas::FunctionContractDeclarerPass());
                            return true;
                        } else if (Name == "pallas-transform-function-body") {
                            FPM.addPass(pallas::FunctionBodyTransformerPass());
                            return true;
                        }
                        return false;
                    });
            }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return getPallasPluginInfo();
}
