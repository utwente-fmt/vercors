#include "Transform/Transform.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionBodyTransformer.h"
#include "Util/Exceptions.h"

/**
 * Utility function that converts LLVM types to col types
 * @param type
 */
const std::string SOURCE_LOC = "Transform::Transform";

namespace col = vct::col::ast;

void llvm2col::transformAndSetType(llvm::Type &llvmType, col::Type &colType) {
    switch (llvmType.getTypeID()) {
    case llvm::Type::IntegerTyID:
        if (llvmType.getIntegerBitWidth() == 1) {
            colType.mutable_t_bool()->set_allocated_origin(
                generateTypeOrigin(llvmType));
        } else {
            colType.mutable_t_int()->set_allocated_origin(
                generateTypeOrigin(llvmType));
        }
        break;
    case llvm::Type::VoidTyID:
        colType.mutable_t_void()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        break;
    case llvm::Type::PointerTyID:
        colType.mutable_llvmt_pointer()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        break;
    case llvm::Type::MetadataTyID:
        colType.mutable_llvmt_metadata()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        break;
    default:
        throw pallas::UnsupportedTypeException(llvmType);
    }
}

void llvm2col::transformAndSetExpr(pallas::FunctionCursor &functionCursor,
                                   llvm::Instruction &llvmInstruction,
                                   llvm::Value &llvmOperand,
                                   col::Expr &colExpr) {
    if (llvm::isa<llvm::Constant>(llvmOperand)) {
        transformAndSetConstExpr(functionCursor, llvmInstruction,
                                 llvm::cast<llvm::Constant>(llvmOperand),
                                 colExpr);
    } else {
        transformAndSetVarExpr(functionCursor, llvmInstruction, llvmOperand,
                               colExpr);
    }
}

void llvm2col::transformAndSetVarExpr(pallas::FunctionCursor &functionCursor,
                                      llvm::Instruction &llvmInstruction,
                                      llvm::Value &llvmOperand,
                                      col::Expr &colExpr) {
    col::Variable colVar = functionCursor.getVariableMapEntry(llvmOperand);
    col::Local *colLocal = colExpr.mutable_local();
    colLocal->set_allocated_origin(
        generateOperandOrigin(llvmInstruction, llvmOperand));
    colLocal->mutable_ref()->set_id(colVar.id());
}

void llvm2col::transformAndSetConstExpr(pallas::FunctionCursor &functionCursor,
                                        llvm::Instruction &llvmInstruction,
                                        llvm::Constant &llvmConstant,
                                        col::Expr &colExpr) {
    llvm::Type *constType = llvmConstant.getType();
    switch (llvmConstant.getType()->getTypeID()) {
    case llvm::Type::IntegerTyID:
        if (constType->getIntegerBitWidth() == 1) {
            col::BooleanValue *boolValue = colExpr.mutable_boolean_value();
            boolValue->set_allocated_origin(
                generateOperandOrigin(llvmInstruction, llvmConstant));
            boolValue->set_value(llvmConstant.isOneValue());
        } else {
            col::IntegerValue *integerValue = colExpr.mutable_integer_value();
            integerValue->set_allocated_origin(
                generateOperandOrigin(llvmInstruction, llvmConstant));
            llvm::APInt apInt = llvmConstant.getUniqueInteger();
            transformAndSetBigInt(apInt, *integerValue->mutable_value());
        }
        break;
    case llvm::Type::PointerTyID: {
        llvm::Value *stripped = llvmConstant.stripPointerCastsAndAliases();
        if (llvm::isa<llvm::Function>(stripped)) {
            col::LlvmFunctionPointerValue *funcPointer =
                colExpr.mutable_llvm_function_pointer_value();
            funcPointer->set_allocated_origin(
                generateOperandOrigin(llvmInstruction, *stripped));
            funcPointer->mutable_value()->set_id(
                functionCursor
                    .getOtherFunction(llvm::cast<llvm::Function>(*stripped))
                    .id());
        } else {
            std::string errCtx;
            llvm::raw_string_ostream(errCtx) << llvmConstant;
            std::stringstream errorStream;
            errorStream << "Unknown constant pointer '" << errCtx << "'";
            pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(),
                                            llvmInstruction);
        }
        break;
    }
    default:
        std::string errCtx;
        llvm::raw_string_ostream(errCtx) << llvmConstant;
        std::stringstream errorStream;
        errorStream << "Unknown constant '" << errCtx << "' of type '"
                    << constType->getTypeID() << "'";
        pallas::ErrorReporter::addError(SOURCE_LOC, errorStream.str(),
                                        llvmInstruction);
    }
}

void llvm2col::transformAndSetBigInt(llvm::APInt &apInt, col::BigInt &bigInt) {
    // TODO works for "small" signed and unsigned numbers, may break for values
    // >=2^64
    llvm::APInt byteSwapped = apInt.byteSwap();
    std::vector<u_int64_t> byteVector;
    for (uint32_t i = 0; i < byteSwapped.getNumWords(); i++) {
        byteVector.push_back(byteSwapped.getRawData()[i]);
    }
    bigInt.set_data(byteVector.data(), apInt.getBitWidth() / 8);
}

std::string llvm2col::getValueName(llvm::Value &llvmValue) {
    std::string name;
    llvm::raw_string_ostream contextStream = llvm::raw_string_ostream(name);
    llvmValue.printAsOperand(contextStream, false);
    return name;
}
