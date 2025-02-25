#include "Passes/Function/FunctionBodyTransformer.h"

#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionContractDeclarer.h"
#include "Passes/Function/FunctionDeclarer.h"
#include "Transform/BlockTransform.h"
#include "Transform/Transform.h"
#include "Util/BlockUtils.h"
#include "Util/Exceptions.h"
#include <llvm/Support/raw_ostream.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Function::FunctionBodyTransformer";

FunctionCursor::FunctionCursor(col::Scope &functionScope,
                               col::Block &functionBody,
                               llvm::Function &llvmFunction,
                               llvm::FunctionAnalysisManager &FAM)
    : functionScope(functionScope), functionBody(functionBody),
      llvmFunction(llvmFunction), FAM(FAM) {}

const col::Scope &FunctionCursor::getFunctionScope() { return functionScope; }

void FunctionCursor::addVariableMapEntry(Value &llvmValue,
                                         col::Variable &colVar) {
    variableMap.insert({&llvmValue, &colVar});
    // add reference to reference lut of function contract
    // (only if the contract is a vcllvm contract)
    col::LlvmFunctionContract &contract =
        FAM.getResult<FunctionContractDeclarer>(llvmFunction)
            .getAssociatedColFuncContract();
    if (contract.has_vcllvm_function_contract()) {
        col::VcllvmFunctionContract *vcllvmContract =
            contract.mutable_vcllvm_function_contract();
        col::Tuple2_String_Ref_VctColAstVariable *ref =
            vcllvmContract->add_variable_refs();
        ref->set_v1(llvm2col::getValueName(llvmValue));
        ref->mutable_v2()->set_id(colVar.id());
    }
}

col::Variable &FunctionCursor::getVariableMapEntry(Value &llvmValue,
                                                   bool inPhiNode) {
    if (auto variablePair = variableMap.find(&llvmValue);
        variablePair != variableMap.end()) {
        return *variablePair->second;
    } else {
        if (!inPhiNode) {
            std::string str;
            llvm::raw_string_ostream output(str);
            output << "Use of undeclared variable: '" << llvmValue << "'";
            ErrorReporter::addError(SOURCE_LOC, str);
        }

        col::Variable *colVar = new col::Variable();
        llvm2col::setColNodeId(colVar);
        addVariableMapEntry(llvmValue, *colVar);
        return *colVar;
    }
}

bool FunctionCursor::isVisited(BasicBlock &llvmBlock) {
    return visitedColBlocks.contains(
        &this->getOrSetLLVMBlock2ColBlockEntry(llvmBlock));
}

col::LlvmBasicBlock &
FunctionCursor::getOrSetLLVMBlock2ColBlockEntry(BasicBlock &llvmBlock) {
    if (!llvmBlock2ColBlock.contains(&llvmBlock)) {
        // create basic block in buffer
        col::LlvmBasicBlock *bb =
            functionBody.add_statements()->mutable_llvm_basic_block();
        // set basic block origin
        bb->set_allocated_origin(llvm2col::generateLabelOrigin(llvmBlock));
        // create label declaration in buffer
        col::LabelDecl *labelDecl = bb->mutable_label();
        // set label decl origin
        labelDecl->set_allocated_origin(
            llvm2col::generateLabelOrigin(llvmBlock));
        // set label decl id
        llvm2col::setColNodeId(labelDecl);
        // create block inside label statement
        col::Block *block = bb->mutable_body()->mutable_block();
        // set block origin
        block->set_allocated_origin(llvm2col::generateBlockOrigin(llvmBlock));
        llvmBlock2ColBlock.insert({&llvmBlock, *bb});
    }
    return llvmBlock2ColBlock.at(&llvmBlock);
}

col::LlvmBasicBlock &FunctionCursor::visitLLVMBlock(BasicBlock &llvmBlock) {
    col::LlvmBasicBlock &labeledBlock =
        this->getOrSetLLVMBlock2ColBlockEntry(llvmBlock);
    visitedColBlocks.insert(&labeledBlock);
    return labeledBlock;
}

LoopInfo &FunctionCursor::getLoopInfo() {
    return FAM.getResult<LoopAnalysis>(llvmFunction);
}

LoopInfo &FunctionCursor::getLoopInfo(Function &otherLLVMFunction) {
    return FAM.getResult<LoopAnalysis>(otherLLVMFunction);
}

FDResult &FunctionCursor::getFDResult() {
    return FAM.getResult<FunctionDeclarer>(llvmFunction);
}

FDResult &FunctionCursor::getFDResult(Function &otherLLVMFunction) {
    return FAM.getResult<FunctionDeclarer>(otherLLVMFunction);
}

col::Variable &FunctionCursor::declareVariable(Instruction &llvmInstruction,
                                               Type *llvmPointerType) {
    col::Variable *varDecl;
    if (auto variablePair = variableMap.find(&llvmInstruction);
        variablePair != variableMap.end()) {
        varDecl = functionScope.add_locals();
        *varDecl = *variablePair->second;
    } else {
        // create declaration in buffer
        varDecl = functionScope.add_locals();
        // set id
        llvm2col::setColNodeId(varDecl);
        // add to the variable lut
        this->addVariableMapEntry(llvmInstruction, *varDecl);
    }
    // set type of declaration
    try {
        if (llvmPointerType == nullptr) {
            llvm2col::transformAndSetType(*llvmInstruction.getType(),
                                          *varDecl->mutable_t());
        } else {
            llvm2col::transformAndSetPointerType(*llvmPointerType,
                                                 *varDecl->mutable_t());
        }
    } catch (pallas::UnsupportedTypeException &e) {
        std::stringstream errorStream;
        errorStream << e.what() << " in variable declaration.";
        ErrorReporter::addError(SOURCE_LOC, errorStream.str(), llvmInstruction);
    }
    // set origin
    varDecl->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmInstruction));
    return *varDecl;
}

col::Assign &
FunctionCursor::createAssignmentAndDeclaration(Instruction &llvmInstruction,
                                               col::LlvmBasicBlock &colBlock,
                                               Type *llvmPointerType) {
    col::Variable &varDecl = declareVariable(llvmInstruction, llvmPointerType);
    return createAssignment(llvmInstruction, colBlock, varDecl);
}

col::Assign &FunctionCursor::createAssignment(Instruction &llvmInstruction,
                                              col::LlvmBasicBlock &colBlock,
                                              col::Variable &varDecl) {
    col::Block &body = pallas::bodyAsBlock(colBlock);
    col::Assign *assignment = body.add_statements()->mutable_assign();
    assignment->set_allocated_blame(new col::Blame());
    assignment->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmInstruction));
    // create local target in buffer and set origin
    col::Local *colLocal = assignment->mutable_target()->mutable_local();
    colLocal->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(llvmInstruction));
    // set target to refer to var decl
    colLocal->mutable_ref()->set_id(varDecl.id());
    return *assignment;
}

col::Assign &FunctionCursor::createPhiAssignment(Instruction &llvmInstruction,
                                                 col::LlvmBasicBlock &colBlock,
                                                 col::Variable &varDecl) {
    // col::Assign *assignment = new col::Assign();
    col::Assign *assignment = colBlock.add_phi_assignments()->mutable_assign();
    assignment->set_allocated_blame(new col::Blame());
    assignment->set_allocated_origin(
        llvm2col::generateSingleStatementOrigin(llvmInstruction));
    // create local target in buffer and set origin
    col::Local *colLocal = assignment->mutable_target()->mutable_local();
    colLocal->set_allocated_origin(
        llvm2col::generateAssignTargetOrigin(llvmInstruction));
    // set target to refer to var decl
    colLocal->mutable_ref()->set_id(varDecl.id());
    return *assignment;
}

llvm::FunctionAnalysisManager &FunctionCursor::getFunctionAnalysisManager() {
    return FAM;
}

PreservedAnalyses
FunctionBodyTransformerPass::run(Function &F, FunctionAnalysisManager &FAM) {
    ColScopedFuncBody scopedFuncBody =
        FAM.getResult<FunctionDeclarer>(F).getAssociatedScopedColFuncBody();
    FunctionCursor funcCursor =
        FunctionCursor(*scopedFuncBody.scope, *scopedFuncBody.block, F, FAM);
    // add function arguments to the variableMap
    for (auto &A : F.args()) {
        funcCursor.addVariableMapEntry(
            A, FAM.getResult<FunctionDeclarer>(F).getFuncArgMapEntry(A));
    }
    // start recursive block code gen with basic block
    llvm::BasicBlock &entryBlock = F.getEntryBlock();
    llvm2col::transformLLVMBlock(entryBlock, funcCursor);
    return PreservedAnalyses::all();
}
} // namespace pallas
