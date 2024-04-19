#include "Transform/Transform.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "Util/Exceptions.h"
#include "Passes/Function/FunctionBodyTransformer.h"
#include "Origin/OriginProvider.h"




/**
 * Utility function that converts LLVM types to col types
 * @param type
 */
namespace llvm2Col {
    const std::string SOURCE_LOC = "Transform::Transform";

    namespace col = vct::col::ast;

    void transformAndSetType(llvm::Type &llvmType,
                             col::Type &colType) {
        switch (llvmType.getTypeID()) {
            case llvm::Type::IntegerTyID:
                if (llvmType.getIntegerBitWidth() == 1) {
                    colType.mutable_t_bool()->set_allocated_origin(generateTypeOrigin(llvmType));
                } else {
                    colType.mutable_t_int()->set_allocated_origin(generateTypeOrigin(llvmType));
                }
                break;
            default:
                throw vcllvm::UnsupportedTypeException();
        }
    }


    void transformAndSetExpr(vcllvm::FunctionCursor &functionCursor,
                             llvm::Instruction &llvmInstruction,
                             llvm::Value &llvmOperand,
                             col::Expr &colExpr) {
        if (llvm::isa<llvm::Constant>(llvmOperand)) {
            transformAndSetConstExpr(llvmInstruction, llvm::cast<llvm::Constant>(llvmOperand), colExpr);
        } else {
            transformAndSetVarExpr(functionCursor, llvmInstruction, llvmOperand, colExpr);
        }
    }

    void transformAndSetVarExpr(vcllvm::FunctionCursor &functionCursor,
                                llvm::Instruction &llvmInstruction,
                                llvm::Value &llvmOperand,
                                col::Expr &colExpr) {
        col::Variable colVar = functionCursor.getVariableMapEntry(llvmOperand);
        col::Local *colLocal = colExpr.mutable_local();
        colLocal->set_allocated_origin(generateOperandOrigin(llvmInstruction, llvmOperand));
        colLocal->mutable_ref()->set_id(colVar.id());
    }

    void transformAndSetConstExpr(llvm::Instruction &llvmInstruction,
                                  llvm::Constant &llvmConstant,
                                  col::Expr &colExpr) {
        llvm::Type *constType = llvmConstant.getType();
        switch (llvmConstant.getType()->getTypeID()) {
            case llvm::Type::IntegerTyID:
                if (constType->getIntegerBitWidth() == 1) {
                    col::BooleanValue *boolValue = colExpr.mutable_boolean_value();
                    boolValue->set_allocated_origin(generateOperandOrigin(llvmInstruction, llvmConstant));
                    boolValue->set_value(llvmConstant.isOneValue());
                } else {
                    col::IntegerValue *integerValue = colExpr.mutable_integer_value();
                    integerValue->set_allocated_origin(generateOperandOrigin(llvmInstruction, llvmConstant));
                    llvm::APInt apInt = llvmConstant.getUniqueInteger();
                    transformAndSetIntegerValue(apInt, *integerValue);
                }
                break;
            default:
                std::string errCtx;
                llvm::raw_string_ostream(errCtx) << llvmConstant;
                std::stringstream errorStream;
                errorStream << "Unknown constant \"" << errCtx << '\"';
                vcllvm::ErrorReporter::addError(SOURCE_LOC, errorStream.str(), llvmInstruction);
        }
    }

    void transformAndSetIntegerValue(llvm::APInt &apInt, col::IntegerValue &colIntegerValue) {
        // TODO works for "small" signed and unsigned numbers, may break for values >=2^64
        llvm::APInt byteSwapped = apInt.byteSwap();
        std::vector<u_int64_t> byteVector;
        for (uint32_t i = 0; i < byteSwapped.getNumWords(); i++) {
            byteVector.push_back(byteSwapped.getRawData()[i]);
        }
        colIntegerValue.mutable_value()->set_data(byteVector.data(), apInt.getBitWidth() / 8);
    }

    std::string getValueName(llvm::Value &llvmValue) {
        std::string name;
        llvm::raw_string_ostream contextStream = llvm::raw_string_ostream(name);
        llvmValue.printAsOperand(contextStream, false);
        return name;
    }
}