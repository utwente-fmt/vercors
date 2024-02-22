#include "Origin/OriginProvider.h"

#include <llvm/IR/Module.h>

#include "Origin/ContextDeriver.h"
#include "Origin/PreferredNameDeriver.h"
#include "Origin/ShortPositionDeriver.h"

namespace col = vct::col::ast;

col::Origin *llvm2col::generateProgramOrigin(llvm::Module &llvmModule) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name("program:" + llvmModule.getName().str());
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveModuleContext(llvmModule));
    context->set_inline_context(deriveModuleContext(llvmModule));
    context->set_short_position(deriveModuleShortPosition(llvmModule));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateFuncDefOrigin(llvm::Function &llvmFunction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(llvmFunction.getName().str());
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveFunctionContext(llvmFunction));
    context->set_inline_context(deriveFunctionContext(llvmFunction));
    context->set_short_position(deriveFunctionShortPosition(llvmFunction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateFunctionContractOrigin(llvm::Function &llvmFunction,
                                         const std::string &contract) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(contract);
    context->set_inline_context(contract);
    context->set_short_position(deriveFunctionShortPosition(llvmFunction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateGlobalValOrigin(llvm::Module &llvmModule,
                                               const std::string &globVal) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(globVal);
    context->set_inline_context(globVal);
    context->set_short_position(deriveModuleShortPosition(llvmModule));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateArgumentOrigin(llvm::Argument &llvmArgument) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(
        deriveArgumentPreferredName(llvmArgument));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveFunctionContext(*llvmArgument.getParent()));
    context->set_inline_context(
        deriveFunctionContext(*llvmArgument.getParent()));
    context->set_short_position(
        deriveFunctionShortPosition(*llvmArgument.getParent()));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateBlockOrigin(llvm::BasicBlock &llvmBlock) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name("block");
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveBlockContext(llvmBlock));
    context->set_inline_context(deriveBlockContext(llvmBlock));
    context->set_short_position(deriveBlockShortPosition(llvmBlock));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateLabelOrigin(llvm::BasicBlock &llvmBlock) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name("label");
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveLabelContext(llvmBlock));
    context->set_inline_context(deriveLabelContext(llvmBlock));
    context->set_short_position(deriveBlockShortPosition(llvmBlock));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateSingleStatementOrigin(llvm::Instruction &llvmInstruction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(
        deriveOperandPreferredName(llvmInstruction));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveSurroundingInstructionContext(llvmInstruction));
    context->set_inline_context(deriveInstructionContext(llvmInstruction));
    context->set_short_position(
        deriveInstructionShortPosition(llvmInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateAssignTargetOrigin(llvm::Instruction &llvmInstruction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name("var");
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveInstructionContext(llvmInstruction));
    context->set_inline_context(deriveInstructionLhs(llvmInstruction));
    context->set_short_position(
        deriveInstructionShortPosition(llvmInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateBinExprOrigin(llvm::Instruction &llvmInstruction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveSurroundingInstructionContext(llvmInstruction));
    context->set_inline_context(deriveInstructionContext(llvmInstruction));
    context->set_short_position(
        deriveInstructionShortPosition(llvmInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateFunctionCallOrigin(llvm::CallInst &callInstruction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(
        callInstruction.getCalledFunction()->getName().str());
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveSurroundingInstructionContext(callInstruction));
    context->set_inline_context(deriveInstructionRhs(callInstruction));
    context->set_short_position(
        deriveInstructionShortPosition(callInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateOperandOrigin(llvm::Instruction &llvmInstruction,
                                             llvm::Value &llvmOperand) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(deriveOperandPreferredName(llvmOperand));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveInstructionContext(llvmInstruction));
    context->set_inline_context(deriveOperandContext(llvmOperand));
    context->set_short_position(
        deriveInstructionShortPosition(llvmInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateGlobalVariableOrigin(
    llvm::Module &llvmModule, llvm::GlobalVariable &llvmGlobalVariable) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(llvmGlobalVariable.getName().str());
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveGlobalVariableContext(llvmGlobalVariable));
    context->set_inline_context("unknown");
    context->set_short_position(deriveModuleShortPosition(llvmModule));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateGlobalVariableInitializerOrigin(
    llvm::Module &llvmModule, llvm::GlobalVariable &llvmGlobalVariable,
    llvm::Value &llvmInitializer) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(
        deriveOperandPreferredName(llvmInitializer));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveGlobalVariableContext(llvmGlobalVariable));
    context->set_inline_context(deriveOperandContext(llvmInitializer));
    context->set_short_position(deriveModuleShortPosition(llvmModule));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *
llvm2col::generateVoidOperandOrigin(llvm::Instruction &llvmInstruction) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name("void");
    preferredNameContent->set_allocated_preferred_name(preferredName);

    col::OriginContent *contextContent = origin->add_content();
    col::Context *context = new col::Context();
    context->set_context(deriveInstructionContext(llvmInstruction));
    context->set_inline_context("void");
    context->set_short_position(
        deriveInstructionShortPosition(llvmInstruction));
    contextContent->set_allocated_context(context);

    return origin;
}

col::Origin *llvm2col::generateTypeOrigin(llvm::Type &llvmType) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(deriveTypePreferredName(llvmType));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    return origin;
}

col::Origin *
llvm2col::generateMemoryOrderingOrigin(llvm::AtomicOrdering &llvmOrdering) {
    col::Origin *origin = new col::Origin();
    col::OriginContent *preferredNameContent = origin->add_content();
    col::PreferredName *preferredName = new col::PreferredName();
    preferredName->add_preferred_name(
        deriveMemoryOrderingPreferredName(llvmOrdering));
    preferredNameContent->set_allocated_preferred_name(preferredName);

    return origin;
}

std::string llvm2col::extractShortPosition(const col::Origin &origin) {
    for (const col::OriginContent &content : origin.content()) {
        if (content.has_context()) {
            return content.context().short_position();
        }
    }
    return "unknown";
}

col::Origin *llvm2col::deepenOperandOrigin(const col::Origin &origin,
                                           llvm::Value &llvmOperand) {
    col::Origin *newOrigin = new col::Origin(origin);

    bool foundName = false;
    bool foundContext = false;
    for (col::OriginContent &content : *newOrigin->mutable_content()) {
        if (content.has_preferred_name()) {
            col::PreferredName *preferredName =
                content.mutable_preferred_name();
            preferredName->clear_preferred_name();
            preferredName->add_preferred_name(
                deriveOperandPreferredName(llvmOperand));
            foundName = true;
        } else if (content.has_context()) {
            content.mutable_context()->set_inline_context(
                deriveOperandContext(llvmOperand));
            foundContext = true;
        }
    }

    if (!foundName) {
        col::PreferredName *preferredName =
            newOrigin->add_content()->mutable_preferred_name();
        preferredName->clear_preferred_name();
        preferredName->add_preferred_name(
            deriveOperandPreferredName(llvmOperand));
    }

    if (!foundContext) {
        col::Context *context = newOrigin->add_content()->mutable_context();
        context->set_context("unknown");
        context->set_inline_context(deriveOperandContext(llvmOperand));
        context->set_short_position("unknown");
    }

    return newOrigin;
}
