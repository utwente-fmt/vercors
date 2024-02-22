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

void llvm2col::transformAndSetPointerType(llvm::Type &llvmType,
                                          col::Type &colType) {
    col::LlvmtPointer *pointerType = colType.mutable_llvmt_pointer();
    pointerType->set_allocated_origin(generateTypeOrigin(llvmType));
    llvm2col::transformAndSetType(llvmType, *pointerType->mutable_inner_type());
}

void llvm2col::transformAndSetType(llvm::Type &llvmType, col::Type &colType) {
    switch (llvmType.getTypeID()) {
    case llvm::Type::IntegerTyID:
        if (llvmType.getIntegerBitWidth() == 1) {
            colType.mutable_t_bool()->set_allocated_origin(
                generateTypeOrigin(llvmType));
        } else {
            col::LlvmtInt *colInt = colType.mutable_llvmt_int();
            colInt->set_bit_width(llvmType.getIntegerBitWidth());
            colInt->set_allocated_origin(generateTypeOrigin(llvmType));
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
    case llvm::Type::StructTyID: {
        llvm::StructType &structType = llvm::cast<llvm::StructType>(llvmType);
        col::LlvmtStruct *colStruct = colType.mutable_llvmt_struct();
        colStruct->set_allocated_origin(generateTypeOrigin(llvmType));
        if (!structType.isLiteral()) {
            // TODO: Instead of storing the name do we want keep only a single
            // instance of the col::LLVMTStruct per non-literal struct type?
            // XXX: This name can be the empty string for unnamed types, and it
            // won't be set for literal types
            colStruct->set_name(structType.getName().str());
        }
        colStruct->set_packed(structType.isPacked());
        for (llvm::Type *element : structType.elements()) {
            llvm2col::transformAndSetType(*element, *colStruct->add_elements());
        }
        break;
    }
    case llvm::Type::ArrayTyID: {
        llvm::ArrayType &arrayType = llvm::cast<llvm::ArrayType>(llvmType);
        col::LlvmtArray *colArray = colType.mutable_llvmt_array();
        colArray->set_allocated_origin(generateTypeOrigin(llvmType));
        llvm2col::transformAndSetType(*arrayType.getElementType(),
                                      *colArray->mutable_element_type());
        colArray->set_num_elements(arrayType.getNumElements());
        break;
    }
    case llvm::Type::FixedVectorTyID:
    case llvm::Type::ScalableVectorTyID: {
        llvm::VectorType &vectorType = llvm::cast<llvm::VectorType>(llvmType);
        col::LlvmtVector *colVector = colType.mutable_llvmt_vector();
        colVector->set_allocated_origin(generateTypeOrigin(llvmType));
        llvm2col::transformAndSetType(*vectorType.getElementType(),
                                      *colVector->mutable_element_type());
        colVector->set_num_elements(
            vectorType.getElementCount().getKnownMinValue());
        break;
    }

    default:
        throw pallas::UnsupportedTypeException(llvmType);
    }
}

void llvm2col::transformAndSetExpr(pallas::FunctionCursor &functionCursor,
                                   llvm::Instruction &llvmInstruction,
                                   llvm::Value &llvmOperand,
                                   col::Expr &colExpr) {
    col::Origin *origin = generateOperandOrigin(llvmInstruction, llvmOperand);
    if (llvm::isa<llvm::Constant>(llvmOperand)) {
        transformAndSetConstExpr(
            functionCursor.getFunctionAnalysisManager(), origin,
            llvm::cast<llvm::Constant>(llvmOperand), colExpr);
    } else {
        transformAndSetVarExpr(functionCursor, origin,
                               llvmInstruction.getOpcode() ==
                                   llvm::Instruction::PHI,
                               llvmOperand, colExpr);
    }
}

void llvm2col::transformAndSetVarExpr(pallas::FunctionCursor &functionCursor,
                                      col::Origin *origin, bool inPhiNode,
                                      llvm::Value &llvmOperand,
                                      col::Expr &colExpr) {
    col::Variable colVar =
        functionCursor.getVariableMapEntry(llvmOperand, inPhiNode);
    col::Local *colLocal = colExpr.mutable_local();
    colLocal->set_allocated_origin(origin);
    colLocal->mutable_ref()->set_id(colVar.id());
}

void llvm2col::transformAndSetConstExpr(llvm::FunctionAnalysisManager &FAM,
                                        col::Origin *origin,
                                        llvm::Constant &llvmConstant,
                                        col::Expr &colExpr) {
    if (llvm::isa<llvm::ConstantAggregateZero>(llvmConstant)) {
        col::LlvmZeroedAggregateValue *colZero =
            colExpr.mutable_llvm_zeroed_aggregate_value();

        colZero->set_allocated_origin(origin);
        llvm2col::transformAndSetType(*llvmConstant.getType(),
                                      *colZero->mutable_aggregate_type());
        return;
    }
    llvm::Type *constType = llvmConstant.getType();
    switch (llvmConstant.getType()->getTypeID()) {
    case llvm::Type::IntegerTyID:
        if (constType->getIntegerBitWidth() == 1) {
            col::BooleanValue *boolValue = colExpr.mutable_boolean_value();
            boolValue->set_allocated_origin(origin);
            boolValue->set_value(llvmConstant.isOneValue());
        } else {
            col::LlvmIntegerValue *integerValue =
                colExpr.mutable_llvm_integer_value();
            integerValue->set_allocated_origin(origin);
            llvm::APInt apInt = llvmConstant.getUniqueInteger();
            transformAndSetBigInt(apInt, *integerValue->mutable_value());
            col::LlvmtInt *colInt =
                integerValue->mutable_integer_type()->mutable_llvmt_int();
            colInt->set_bit_width(constType->getIntegerBitWidth());
            colInt->set_allocated_origin(generateTypeOrigin(*constType));
        }
        break;
    case llvm::Type::PointerTyID: {
        // Can't be a function since we caught that in transformAndSetExpr
        llvm::Value *stripped = llvmConstant.stripPointerCastsAndAliases();
        if (llvm::isa<llvm::Function>(stripped)) {
            col::LlvmFunctionPointerValue *funcPointer =
                colExpr.mutable_llvm_function_pointer_value();
            funcPointer->set_allocated_origin(origin);
            funcPointer->mutable_value()->set_id(
                FAM.getResult<pallas::FunctionDeclarer>(
                       llvm::cast<llvm::Function>(*stripped))
                    .getAssociatedColFuncDef()
                    .id());
        } else if (llvm::isa<llvm::GlobalVariable>(stripped)) {
            // XXX: To avoid having a map of GlobalVariables to their COL nodes
            // we break with the convention and use the memory location of the
            // LLVM value instead of the memory location of the COL node as the
            // id
            auto id = reinterpret_cast<int64_t>(stripped);
            col::LlvmPointerValue *pointer =
                colExpr.mutable_llvm_pointer_value();
            pointer->set_allocated_origin(origin);
            pointer->mutable_value()->set_id(id);
        } else if (llvm::isa<llvm::ConstantPointerNull>(stripped)) {
            col::Null *pointer = colExpr.mutable_null();
            pointer->set_allocated_origin(origin);
        } else {
            std::string errCtx;
            llvm::raw_string_ostream(errCtx) << llvmConstant;
            std::stringstream errorStream;
            errorStream << "Unknown constant pointer '" << errCtx << "' "
                        << llvm::isa<llvm::ConstantStruct>(stripped) << ", "
                        << llvm::isa<llvm::ConstantVector>(stripped) << ", "
                        << llvm::isa<llvm::ConstantArray>(stripped) << ", "
                        << llvm::isa<llvm::ConstantDataArray>(stripped) << ", "
                        << llvm::isa<llvm::ConstantDataVector>(stripped) << ", "
                        << llvm::isa<llvm::GlobalVariable>(stripped);
            pallas::ErrorReporter::addError(
                SOURCE_LOC, errorStream.str(),
                llvm2col::extractShortPosition(*origin));
        }
        break;
    }
    case llvm::Type::StructTyID: {
        llvm::ConstantStruct &llvmStruct =
            llvm::cast<llvm::ConstantStruct>(llvmConstant);
        col::LlvmStructValue *colStruct = colExpr.mutable_llvm_struct_value();

        for (auto &operand : llvmStruct.operands()) {
            llvm2col::transformAndSetConstExpr(
                FAM, llvm2col::deepenOperandOrigin(*origin, *operand.get()),
                llvm::cast<llvm::Constant>(*operand.get()),
                *colStruct->add_value());
        }
        colStruct->set_allocated_origin(origin);
        llvm2col::transformAndSetType(*llvmStruct.getType(),
                                      *colStruct->mutable_struct_type());

        break;
    }
    case llvm::Type::ArrayTyID: {
        if (llvm::isa<llvm::ConstantArray>(llvmConstant)) {
            llvm::ConstantArray &llvmArray =
                llvm::cast<llvm::ConstantArray>(llvmConstant);
            col::LlvmArrayValue *colArray = colExpr.mutable_llvm_array_value();

            for (auto &operand : llvmArray.operands()) {
                llvm2col::transformAndSetConstExpr(
                    FAM, llvm2col::deepenOperandOrigin(*origin, *operand.get()),
                    llvm::cast<llvm::Constant>(*operand.get()),
                    *colArray->add_value());
            }
            colArray->set_allocated_origin(origin);
            llvm2col::transformAndSetType(*llvmArray.getType(),
                                          *colArray->mutable_array_type());
        } else {
            llvm::ConstantDataArray &llvmArray =
                llvm::cast<llvm::ConstantDataArray>(llvmConstant);
            col::LlvmRawArrayValue *colArray =
                colExpr.mutable_llvm_raw_array_value();

            // TODO: This is not a very useful format. Ideally we detect the
            // type and get elements individually as integers or floats or
            // something
            colArray->set_value(llvmArray.getRawDataValues().str());
            colArray->set_allocated_origin(origin);
            llvm::errs() << "Array constant " << llvmArray << " has type "
                         << *llvmArray.getType() << "\n";
            llvm2col::transformAndSetType(*llvmArray.getType(),
                                          *colArray->mutable_array_type());
        }

        break;
    }
    case llvm::Type::FixedVectorTyID: {
        if (llvm::isa<llvm::ConstantVector>(llvmConstant)) {
            llvm::ConstantVector &llvmVector =
                llvm::cast<llvm::ConstantVector>(llvmConstant);
            col::LlvmVectorValue *colVector =
                colExpr.mutable_llvm_vector_value();

            for (auto &operand : llvmVector.operands()) {
                llvm2col::transformAndSetConstExpr(
                    FAM, llvm2col::deepenOperandOrigin(*origin, *operand.get()),
                    llvm::cast<llvm::Constant>(*operand.get()),
                    *colVector->add_value());
            }
            colVector->set_allocated_origin(origin);
            llvm2col::transformAndSetType(*llvmVector.getType(),
                                          *colVector->mutable_vector_type());
        } else {
            llvm::ConstantDataVector &llvmVector =
                llvm::cast<llvm::ConstantDataVector>(llvmConstant);
            col::LlvmRawVectorValue *colVector =
                colExpr.mutable_llvm_raw_vector_value();

            // TODO: This is not a very useful format. Ideally we detect the
            // type and get elements individually as integers or floats or
            // something
            colVector->set_value(llvmVector.getRawDataValues().str());
            colVector->set_allocated_origin(origin);
            llvm2col::transformAndSetType(*llvmVector.getType(),
                                          *colVector->mutable_vector_type());
        }

        break;
    }
    default:
        std::string errCtx;
        llvm::raw_string_ostream(errCtx) << llvmConstant;
        std::stringstream errorStream;
        errorStream << "Unknown constant '" << errCtx << "' of type '"
                    << constType->getTypeID() << "'";
        pallas::ErrorReporter::addError(
            SOURCE_LOC, errorStream.str(),
            llvm2col::extractShortPosition(*origin));
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
