#include "Origin/PreferredNameDeriver.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Argument.h>

namespace llvm2Col {
    std::string deriveOperandPreferredName(llvm::Value &llvmOperand) {
        std::string preferredName;
        llvm::raw_string_ostream preferredNameStream = llvm::raw_string_ostream(preferredName);
        preferredNameStream << (llvm::isa<llvm::Constant>(llvmOperand) ? "const_" : "var_");
        llvmOperand.printAsOperand(preferredNameStream, false);
        return preferredName;
    }

    std::string deriveTypePreferredName(llvm::Type &llvmType) {
        std::string prefix = "t_";
        switch(llvmType.getTypeID()) {
            case llvm::Type::HalfTyID:
                return prefix + "half";
            case llvm::Type::BFloatTyID:
                return prefix + "bfloat";
            case llvm::Type::FloatTyID:
                return prefix + "float";
            case llvm::Type::DoubleTyID:
                return prefix + "double";
            case llvm::Type::X86_FP80TyID:
                return prefix + "x86fp80";
            case llvm::Type::FP128TyID:
                return prefix + "fp128";
            case llvm::Type::PPC_FP128TyID:
                return prefix + "ppcfp128";
            case llvm::Type::VoidTyID:
                return prefix + "void";
            case llvm::Type::LabelTyID:
                return prefix + "label";
            case llvm::Type::MetadataTyID:
                return prefix + "metadata";
            case llvm::Type::X86_MMXTyID:
                return prefix + "x86mmx";
            case llvm::Type::X86_AMXTyID:
                return prefix + "x86amx";
            case llvm::Type::TokenTyID:
                return prefix + "token";
            case llvm::Type::IntegerTyID:
                return prefix + (llvmType.getIntegerBitWidth() == 1 ? "boolean" : "integer");
            case llvm::Type::FunctionTyID:
                return prefix + "function";
            case llvm::Type::PointerTyID:
                return prefix + "ptr";
            case llvm::Type::StructTyID:
                return prefix + "struct";
            case llvm::Type::ArrayTyID:
                return prefix + "array";
            case llvm::Type::FixedVectorTyID:
                return prefix + "fixedvector";
            case llvm::Type::ScalableVectorTyID:
                return prefix + "scalevector";
        }
    }
        std::string deriveArgumentPreferredName(llvm::Argument &llvmArgument) {
        std::string preferredName;
        llvm::raw_string_ostream preferredNameStream = llvm::raw_string_ostream(preferredName);
        preferredNameStream << "arg_";
        llvmArgument.printAsOperand(preferredNameStream, false);
        return preferredName;
    }

}