#include <llvm/IR/PassManager.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CommandLine.h>

#include "vct/col/ast/col.pb.h"

#include "Passes/Function/FunctionBodyTransformer.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Passes/Function/PureAssigner.h"
#include "Passes/Module/ModuleSpecCollector.h"

#include "Transform/Transform.h"
#include "Origin/OriginProvider.h"

#include "Util/Exceptions.h"

#include <iostream>
#include <memory>

namespace col = vct::col::ast;

col::Program sampleCol(bool returnBool) {
    col::Program program = col::Program();

    // class
    col::GlobalDeclaration *classDeclaration = program.add_declarations();
    col::VctClass *vctClass = classDeclaration->mutable_vct_class();
    llvm2Col::setColNodeId(vctClass);
    col::BooleanValue *lockInvariant = vctClass->mutable_intrinsic_lock_invariant()->mutable_boolean_value();
    lockInvariant->set_value(true);
    // class>method
    col::ClassDeclaration *methodDeclaration = vctClass->add_declarations();
    col::InstanceMethod *method = methodDeclaration->mutable_instance_method();
    llvm2Col::setColNodeId(method);
    // class>method>return_type
    method->mutable_return_type()->mutable_t_bool();
    // class>method>body
    col::Block *body = method->mutable_body()->mutable_scope()->mutable_body()->mutable_block();
    col::Return *returnStatement = body->add_statements()->mutable_return_();
    col::BooleanValue *returnValue = returnStatement->mutable_result()->mutable_boolean_value();
    returnValue->set_value(returnBool);
    // class>method>inline
    method->set_inline_(false);
    // class>method>pure
    method->set_pure(false);
    // class>method>contract
    col::ApplicableContract *contract = method->mutable_contract();
    // class>method>contract>precondition
    col::UnitAccountedPredicate *precondition = contract->mutable_requires_()->mutable_unit_accounted_predicate();
    col::BooleanValue *prePred = precondition->mutable_pred()->mutable_boolean_value();
    prePred->set_value(true);
    // class>method>contract>postcondition
    col::UnitAccountedPredicate *postcondition = contract->mutable_ensures()->mutable_unit_accounted_predicate();
    col::Ref *postRefResult = postcondition->mutable_pred()->mutable_result()->mutable_applicable();
    postRefResult->set_id(method->id());
    // class>method>contract>context_everywhere
    col::BooleanValue *contextEverywhere = contract->mutable_context_everywhere()->mutable_boolean_value();
    contextEverywhere->set_value(true);
    return program;
}

static vcllvm::cl::opt<std::string> inputFileName{"",
                                                  vcllvm::cl::desc{"Module to analyze"},
                                                  vcllvm::cl::value_desc{"IR filename"},
                                                  vcllvm::cl::Positional};
static vcllvm::cl::opt<bool> testCol{"sample-col",
                                     vcllvm::cl::desc{"Output a sample col buffer with verdict PASS"}};

static vcllvm::cl::opt<bool> incorrectTestCol{"sample-col-wrong",
                                              vcllvm::cl::desc{"Output a sample col buffer with verdict FAIL"}};

static vcllvm::cl::opt<bool> humanReadableOutput{"human-readable",
                                              vcllvm::cl::desc{"Output COL buffer in human readable format"}};

int main(int argc, char **argv) {
    vcllvm::cl::ParseCommandLineOptions(argc, argv);
    // sample mode
    if (testCol.getValue() || incorrectTestCol.getValue()) {
        std::cout << sampleCol(testCol.getValue()).SerializeAsString();
        std::cout.flush();
        return EXIT_SUCCESS;
    }
    // parse mode
    if (inputFileName.empty()) {
        vcllvm::errs() << "no input file given\n";
        return EXIT_FAILURE;
    }
    vcllvm::LLVMContext context;
    vcllvm::SMDiagnostic smDiag;
    auto pModule = parseIRFile(inputFileName, smDiag, context);
    if (!pModule) {
        smDiag.print(inputFileName.c_str(), vcllvm::errs());
        return EXIT_FAILURE;
    }
    pModule->setSourceFileName(inputFileName);
    vcllvm::Module *module = pModule.release();
    auto pProgram = std::make_shared<col::Program>();
    // set program origin
    pProgram->set_allocated_origin(llvm2Col::generateProgramOrigin(*module));
    pProgram->set_allocated_blame(new col::Blame());
    // Create the analysis managers.
    vcllvm::LoopAnalysisManager LAM;
    vcllvm::FunctionAnalysisManager FAM;
    vcllvm::CGSCCAnalysisManager CGAM;
    vcllvm::ModuleAnalysisManager MAM;
    FAM.registerPass([&] { return vcllvm::FunctionDeclarer(pProgram); });
    FAM.registerPass([&] { return vcllvm::FunctionContractDeclarer(pProgram); });
    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    vcllvm::PassBuilder PB;
    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    vcllvm::LoopPassManager LPM;

    vcllvm::FunctionPassManager FPM;
    FPM.addPass(vcllvm::FunctionDeclarerPass(pProgram));
    FPM.addPass(vcllvm::PureAssignerPass(pProgram));
    FPM.addPass(vcllvm::FunctionContractDeclarerPass(pProgram));
    FPM.addPass(vcllvm::FunctionBodyTransformerPass(pProgram));
    vcllvm::ModulePassManager MPM;
    MPM.addPass(vcllvm::ModuleSpecCollectorPass(pProgram));
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    MPM.run(*module, MAM);
    if (vcllvm::ErrorReporter::hasErrors()) {
        vcllvm::errs() << "While processing \"" << inputFileName << "\" VCLLVM has encountered "
                       << vcllvm::ErrorReporter::getErrorCount() << " error(s).\n"
                       << "Exiting with failure code...\n";
        return EXIT_FAILURE;
    }
    if (humanReadableOutput.getValue()) {
        llvm::errs() << pProgram->DebugString();
    } else {
        std::cout << pProgram->SerializeAsString();
    }
    return EXIT_SUCCESS;
}
