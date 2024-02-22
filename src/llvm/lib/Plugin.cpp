#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include "Passes/Function/FunctionBodyTransformer.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Passes/Function/PureAssigner.h"
#include "Passes/Module/ModuleSpecCollector.h"
#include "Passes/Module/RootContainer.h"
#include "Passes/Module/ProtobufPrinter.h"

using namespace llvm;

llvm::PassPluginLibraryInfo getVCLLVMPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "VCLLVM", LLVM_VERSION_STRING,
        [](PassBuilder &PB) {
            PB.registerAnalysisRegistrationCallback([](llvm::ModuleAnalysisManager &MAM) {
                MAM.registerPass([&] { return vcllvm::RootContainer(); });
            });
            PB.registerAnalysisRegistrationCallback([](llvm::FunctionAnalysisManager &FAM) {
                FAM.registerPass([&] { return vcllvm::FunctionDeclarer(); });
                FAM.registerPass([&] { return vcllvm::FunctionContractDeclarer(); });
            });
            PB.registerPipelineParsingCallback([](StringRef Name, llvm::ModulePassManager &MPM, ArrayRef<llvm::PassBuilder::PipelineElement>) {
                if (Name == "vcllvm-collect-module-spec") {
                    MPM.addPass(vcllvm::ModuleSpecCollectorPass());
                    return true;
                } else if (Name == "vcllvm-print-protobuf") {
                    MPM.addPass(vcllvm::ProtobufPrinter());
                    return true;
                }
                return false;
            });
            PB.registerPipelineParsingCallback([](StringRef Name, llvm::FunctionPassManager &FPM, ArrayRef<llvm::PassBuilder::PipelineElement>) {
                if (Name == "vcllvm-declare-function") {
                    FPM.addPass(vcllvm::FunctionDeclarerPass());
                    return true;
                } else if (Name == "vcllvm-assign-pure") {
                    FPM.addPass(vcllvm::PureAssignerPass());
                    return true;
                } else if (Name == "vcllvm-declare-function-contract") {
                    FPM.addPass(vcllvm::FunctionContractDeclarerPass());
                    return true;
                } else if (Name == "vcllvm-transform-function-body") {
                    FPM.addPass(vcllvm::FunctionBodyTransformerPass());
                    return true;
                }
                return false;
            });
        }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return getVCLLVMPluginInfo();
}
