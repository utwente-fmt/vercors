#include "Transform/Transform.h"
#include "Origin/OriginProvider.h"
#include "Passes/Function/FunctionBodyTransformer.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"
#include "vct/col/ast/col.pb.h"

#include <llvm/ADT/STLExtras.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

/**
 * Utility function that converts LLVM types to col types
 * @param type
 */
const std::string SOURCE_LOC = "Transform::Transform";

#define WARN_DI_TYPE_MISMATCH(mismatch, typeName)                              \
    pallas::ErrorReporter::addWarning(                                         \
        SOURCE_LOC,                                                            \
        "Debug type mismatch with LLVM type (" mismatch                        \
        "), falling back to LLVM type",                                        \
        typeName)
#define WARN_DI_TYPE_MISSING_SUPPORT(typeType, typeName)                       \
    pallas::ErrorReporter::addWarning(                                         \
        SOURCE_LOC,                                                            \
        "Missing support for debug type for " typeType                         \
        ", falling back to LLVM type",                                         \
        typeName);

void llvm2col::transformAndSetPointerType(llvm::Type &llvmType,
                                          col::Type &colType,
                                          const llvm::DataLayout &dataLayout) {
    col::LlvmtPointer *pointerType = colType.mutable_llvmt_pointer();
    pointerType->set_allocated_origin(generateTypeOrigin(llvmType));
    llvm2col::transformAndSetType(llvmType, *pointerType->mutable_inner_type(),
                                  dataLayout);
}
bool llvm2col::transformAndSetBasicTypeWithDebugInfo(
    llvm::Type *llvmType, llvm::DIBasicType &basicType, col::Type &colType,
    const llvm::DataLayout &dataLayout) {
    switch (basicType.getEncoding()) {
    case llvm::dwarf::DW_ATE_boolean:
        if (llvmType != nullptr &&
            llvmType->getTypeID() != llvm::Type::IntegerTyID) {
            WARN_DI_TYPE_MISMATCH("boolean != integer",
                                  basicType.getName().str());
            return false;
        }

        colType.mutable_t_bool()->set_allocated_origin(
            generateDITypeOrigin(basicType));
        return true;
    case llvm::dwarf::DW_ATE_address:
        WARN_DI_TYPE_MISSING_SUPPORT("segmented addresses",
                                     basicType.getName().str());
        return false;

    // Integers:
    case llvm::dwarf::DW_ATE_signed:
    case llvm::dwarf::DW_ATE_signed_char:
    case llvm::dwarf::DW_ATE_unsigned:
    case llvm::dwarf::DW_ATE_unsigned_char:
    case llvm::dwarf::DW_ATE_signed_fixed:
    case llvm::dwarf::DW_ATE_unsigned_fixed:
    case llvm::dwarf::DW_ATE_UTF:
    case llvm::dwarf::DW_ATE_UCS:
    case llvm::dwarf::DW_ATE_ASCII: {
        if (llvmType != nullptr &&
            llvmType->getTypeID() != llvm::Type::IntegerTyID) {
            WARN_DI_TYPE_MISMATCH("integer != integer",
                                  basicType.getName().str());
            return false;
        }
        if (llvmType != nullptr &&
            llvmType->getIntegerBitWidth() != basicType.getSizeInBits()) {
            WARN_DI_TYPE_MISMATCH("integer bit widths",
                                  basicType.getName().str());
            return false;
        }
        col::LlvmtInt *colInt = colType.mutable_llvmt_int();
        colInt->set_bit_width(basicType.getSizeInBits());
        colInt->set_allocated_origin(generateDITypeOrigin(basicType));
        return true;
    }

    // Floats:
    case llvm::dwarf::DW_ATE_float: {
        if (llvmType == nullptr) {
            switch (basicType.getSizeInBits()) {
            case 16: {
                col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
                colFloat->mutable_float_type()
                    ->mutable_f16()
                    ->set_allocated_origin(generateDITypeOrigin(basicType));
                colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
                return true;
            }
            case 32: {
                col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
                colFloat->mutable_float_type()
                    ->mutable_f32()
                    ->set_allocated_origin(generateDITypeOrigin(basicType));
                colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
                return true;
            }
            case 64: {
                col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
                colFloat->mutable_float_type()
                    ->mutable_f64()
                    ->set_allocated_origin(generateDITypeOrigin(basicType));
                colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
                return true;
            }
            case 80: {
                col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
                colFloat->mutable_float_type()
                    ->mutable_f80()
                    ->set_allocated_origin(generateDITypeOrigin(basicType));
                colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
                return true;
            }
            case 128: {
                col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
                colFloat->mutable_float_type()
                    ->mutable_f128()
                    ->set_allocated_origin(generateDITypeOrigin(basicType));
                colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
                return true;
            }
            default:
                pallas::ErrorReporter::addError(
                    "We do not support floats without LLVM type with special "
                    "types",
                    basicType.getName().str());
                return false;
            }
        }
        switch (llvmType->getTypeID()) {
        case llvm::Type::HalfTyID: {
            if (basicType.getSizeInBits() != 16) {
                WARN_DI_TYPE_MISMATCH("float width != 16",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()->mutable_f16()->set_allocated_origin(
                generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::BFloatTyID: {
            if (basicType.getSizeInBits() != 16) {
                WARN_DI_TYPE_MISMATCH("float width != 16",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()
                ->mutable_b_f16()
                ->set_allocated_origin(generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::FloatTyID: {
            if (basicType.getSizeInBits() != 32) {
                WARN_DI_TYPE_MISMATCH("float width != 32",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()->mutable_f32()->set_allocated_origin(
                generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::DoubleTyID: {
            if (basicType.getSizeInBits() != 64) {
                WARN_DI_TYPE_MISMATCH("float width != 64",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()->mutable_f64()->set_allocated_origin(
                generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::X86_FP80TyID: {
            if (basicType.getSizeInBits() != 80) {
                WARN_DI_TYPE_MISMATCH("float width != 80",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()->mutable_f80()->set_allocated_origin(
                generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::FP128TyID: {
            if (basicType.getSizeInBits() != 128) {
                WARN_DI_TYPE_MISMATCH("float width != 128",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()
                ->mutable_f128()
                ->set_allocated_origin(generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        case llvm::Type::PPC_FP128TyID: {
            if (basicType.getSizeInBits() != 128) {
                WARN_DI_TYPE_MISMATCH("float width != 128",
                                      basicType.getName().str());
                return false;
            }
            col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
            colFloat->mutable_float_type()
                ->mutable_ppc_f128()
                ->set_allocated_origin(generateDITypeOrigin(basicType));
            colFloat->set_allocated_origin(generateDITypeOrigin(basicType));
            return true;
        }
        default:
            WARN_DI_TYPE_MISMATCH("float != float", basicType.getName().str());
            return false;
        }
    }
    case llvm::dwarf::DW_ATE_complex_float:
    case llvm::dwarf::DW_ATE_imaginary_float:
    case llvm::dwarf::DW_ATE_decimal_float:
        WARN_DI_TYPE_MISSING_SUPPORT("special floats",
                                     basicType.getName().str());
        return false;

    // Strings:
    case llvm::dwarf::DW_ATE_packed_decimal:
    case llvm::dwarf::DW_ATE_numeric_string:
    case llvm::dwarf::DW_ATE_edited:
        WARN_DI_TYPE_MISSING_SUPPORT("special strings",
                                     basicType.getName().str());
        return false;

    default:
        WARN_DI_TYPE_MISSING_SUPPORT("unknown type", basicType.getName().str());
        return false;
    }
}
bool llvm2col::transformAndSetCompositeTypeWithDebugInfo(
    llvm::Type *llvmType, llvm::DICompositeType &compositeType,
    col::Type &colType, const llvm::DataLayout &dataLayout) {
    switch (compositeType.getTag()) {
    case llvm::dwarf::DW_TAG_array_type: {
        if (llvmType != nullptr &&
            llvmType->getTypeID() != llvm::Type::ArrayTyID &&
            llvmType->getTypeID() != llvm::Type::PointerTyID) {
            WARN_DI_TYPE_MISMATCH("array != array",
                                  compositeType.getName().str());
            return false;
        }
        if (compositeType.getElements().size() != 1) {
            WARN_DI_TYPE_MISSING_SUPPORT("arrays with more than one subrange",
                                         compositeType.getName().str());
            return false;
        }

        llvm::PointerUnion<llvm::ConstantInt *, llvm::DIVariable *,
                           llvm::DIExpression *>
            count = llvm::cast<llvm::DISubrange>(
                        compositeType.getElements()->getOperand(0))
                        ->getCount();

        if (!count.is<llvm::ConstantInt *>()) {
            WARN_DI_TYPE_MISSING_SUPPORT("arrays with dynamic size",
                                         compositeType.getName().str());
            return false;
        }

        if (llvmType != nullptr &&
            llvmType->getTypeID() == llvm::Type::ArrayTyID &&
            llvmType->getArrayNumElements() !=
                count.get<llvm::ConstantInt *>()->getValue()) {
            WARN_DI_TYPE_MISMATCH("array sizes", compositeType.getName().str());
            return false;
        }

        llvm::Type *childType = nullptr;
        if (llvmType != nullptr &&
            llvmType->getTypeID() == llvm::Type::ArrayTyID) {
            childType = llvmType->getArrayElementType();
        }

        col::LlvmtArray *colArray = colType.mutable_llvmt_array();
        colArray->set_allocated_origin(generateDITypeOrigin(compositeType));

        transformAndSetTypeWithDebugInfo(childType, compositeType.getBaseType(),
                                         *colArray->mutable_element_type(),
                                         dataLayout);
        colArray->set_num_elements(
            count.get<llvm::ConstantInt *>()->getLimitedValue());
        return true;
    }
    case llvm::dwarf::DW_TAG_class_type:
    case llvm::dwarf::DW_TAG_structure_type: {
        col::LlvmtStruct *colStruct = colType.mutable_llvmt_struct();
        colStruct->set_allocated_origin(generateDITypeOrigin(compositeType));
        colStruct->add_name(compositeType.getName().str());
        colStruct->set_size_in_bits(compositeType.getSizeInBits());
        colStruct->set_is_literal(false);
        // TODO: Fix packed vs unpacked, try to detect based on size or get rid
        // of packed as a thing entirely!
        colStruct->set_packed(false);
        if (llvmType == nullptr) {
            std::vector<llvm::DIDerivedType *> elements;
            elements.reserve(compositeType.getElements().size());
            for (auto *element : compositeType.getElements()) {
                assert(llvm::isa<llvm::DIDerivedType>(element));
                if (element->getTag() == llvm::dwarf::DW_TAG_member) {
                    elements.push_back(cast<llvm::DIDerivedType>(element));
                }
            }

            llvm::sort(elements,
                       [&](llvm::DIDerivedType *a, llvm::DIDerivedType *b) {
                           return a->getOffsetInBits() <= b->getOffsetInBits();
                       });

            for (auto *member : elements) {
                col::LlvmFieldDefinition *field = colStruct->add_elements();
                field->set_offset(member->getOffsetInBits());
                field->set_size(
                    pallas::utils::stripIgnored(member->getBaseType())
                        ->getSizeInBits());
                field->set_allocated_origin(
                    generateStructMemberOrigin(*member));
                llvm2col::transformAndSetTypeWithDebugInfo(
                    nullptr, member->getBaseType(), *field->mutable_t(),
                    dataLayout);
            }

            return true;
        }

        if (llvmType->getTypeID() != llvm::Type::StructTyID) {
            llvmType->dump();
            compositeType.dump();
            WARN_DI_TYPE_MISMATCH("struct != struct",
                                  compositeType.getName().str());
            return false;
        }

        auto structType = cast<llvm::StructType>(llvmType);
        if (structType->hasName()) {
            colStruct->add_name(structType->getName().str());
        }
        std::vector<std::tuple<uint64_t, llvm::DIDerivedType *, llvm::Type *>>
            elements;
        elements.reserve(structType->getNumElements());

        const llvm::StructLayout *structLayout =
            dataLayout.getStructLayout(structType);
        for (size_t i = 0, end = structType->getNumElements(); i < end; ++i) {
            elements.push_back({structLayout->getElementOffsetInBits(i),
                                nullptr, structType->getElementType(i)});
        }

        for (auto *element : compositeType.getElements()) {
            assert(llvm::isa<llvm::DIDerivedType>(element));
            if (element->getTag() == llvm::dwarf::DW_TAG_member) {
                auto *member = cast<llvm::DIDerivedType>(element);
                size_t i = 0;
                for (size_t end = elements.size(); i < end; ++i) {
                    auto &[offset, diMember, _llvmMember] = elements[i];
                    assert(member->getOffsetInBits() >= offset &&
                           "DIStruct member type at offset not in original "
                           "struct!");
                    if (member->getOffsetInBits() == offset) {
                        assert(diMember == nullptr);
                        diMember = member;
                        break;
                    }
                }
                assert(i != elements.size() ||
                       member->getOffsetInBits() == std::get<0>(elements[i]));
            }
        }

        for (auto &[offset, diMember, llvmMember] : elements) {
            col::LlvmFieldDefinition *field = colStruct->add_elements();
            field->set_offset(offset);
            field->set_size(dataLayout.getTypeSizeInBits(llvmMember));
            if (diMember == nullptr) {
                field->set_allocated_origin(generateTypeOrigin(*llvmMember));
            } else {
                field->set_allocated_origin(
                    generateStructMemberOrigin(*diMember));
            }
            llvm2col::transformAndSetTypeWithDebugInfo(
                llvmMember,
                diMember == nullptr ? nullptr : diMember->getBaseType(),
                *field->mutable_t(), dataLayout);
        }

        return true;
    }
    case llvm::dwarf::DW_TAG_inheritance:
        // TODO: Inheritance
        WARN_DI_TYPE_MISSING_SUPPORT("inheritance",
                                     compositeType.getName().str());
        return false;
    case llvm::dwarf::DW_TAG_enumeration_type:
        // TODO: Enums (may also be a class if we have AT_enum_class or if the
        // language mandates it)
        WARN_DI_TYPE_MISSING_SUPPORT("enums", compositeType.getName().str());
        return false;
    case llvm::dwarf::DW_TAG_union_type:
    case llvm::dwarf::DW_TAG_variant:
        // TODO: Variants (i.e. rust-style enums)
        WARN_DI_TYPE_MISSING_SUPPORT("variants and unions",
                                     compositeType.getName().str());
        return false;
    case llvm::dwarf::DW_TAG_subrange_type:
        // TODO: Slices?
        WARN_DI_TYPE_MISSING_SUPPORT("subranges",
                                     compositeType.getName().str());
        return false;
    default:
        WARN_DI_TYPE_MISSING_SUPPORT("composite type with unknown tag",
                                     compositeType.getName().str());
        return false;
    }
}
bool llvm2col::transformAndSetDerivedTypeWithDebugInfo(
    llvm::Type *llvmType, llvm::DIDerivedType &derivedType, col::Type &colType,
    const llvm::DataLayout &dataLayout) {
    switch (derivedType.getTag()) {
    case llvm::dwarf::DW_TAG_pointer_type:
    case llvm::dwarf::DW_TAG_reference_type: {
        if (llvmType != nullptr &&
            llvmType->getTypeID() != llvm::Type::PointerTyID) {
            WARN_DI_TYPE_MISMATCH("pointer != pointer",
                                  derivedType.getName().str());
            return false;
        }

        col::LlvmtPointer *colPointer = colType.mutable_llvmt_pointer();
        colPointer->set_allocated_origin(generateDITypeOrigin(derivedType));

        if (derivedType.getBaseType() != nullptr) {
            transformAndSetTypeWithDebugInfo(nullptr, derivedType.getBaseType(),
                                             *colPointer->mutable_inner_type(),
                                             dataLayout);
        }

        return true;
    }
    case llvm::dwarf::DW_TAG_typedef:
    case llvm::dwarf::DW_TAG_const_type:
    case llvm::dwarf::DW_TAG_volatile_type:
    case llvm::dwarf::DW_TAG_shared_type:
    case llvm::dwarf::DW_TAG_atomic_type:
    case llvm::dwarf::DW_TAG_immutable_type:
        transformAndSetTypeWithDebugInfo(llvmType, derivedType.getBaseType(),
                                         colType, dataLayout);
        return true;
    default:
        WARN_DI_TYPE_MISSING_SUPPORT("derived type with unknown tag",
                                     derivedType.getName().str());
        return false;
    }
}

void llvm2col::transformAndSetTypeWithDebugInfo(
    llvm::Type *llvmType, llvm::DIType *debugType, col::Type &colType,
    const llvm::DataLayout &dataLayout) {
    if (debugType == nullptr) {
        if (llvmType == nullptr ||
            llvmType->getTypeID() == llvm::Type::VoidTyID) {
            colType.mutable_t_void()->set_allocated_origin(new col::Origin{});
            return;
        }
        pallas::ErrorReporter::addWarning(
            SOURCE_LOC, "Debug type mismatch with LLVM type (null != void), "
                        "falling back to LLVM type");
    } else if (auto *basicType = dyn_cast<llvm::DIBasicType>(debugType)) {
        if (transformAndSetBasicTypeWithDebugInfo(llvmType, *basicType, colType,
                                                  dataLayout))
            return;
    } else if (auto *compositeType =
                   dyn_cast<llvm::DICompositeType>(debugType)) {
        if (transformAndSetCompositeTypeWithDebugInfo(llvmType, *compositeType,
                                                      colType, dataLayout))
            return;
    } else if (auto *derivedType = dyn_cast<llvm::DIDerivedType>(debugType)) {
        if (transformAndSetDerivedTypeWithDebugInfo(llvmType, *derivedType,
                                                    colType, dataLayout))
            return;
    } else if (auto *stringType = dyn_cast<llvm::DIStringType>(debugType)) {
    } else if (auto *subroutineType =
                   dyn_cast<llvm::DISubroutineType>(debugType)) {
        // } else if (auto *subrangeType =
        // dyn_cast<llvm::DISubrangeType>(debugType)) {
    } else {
        pallas::ErrorReporter::addWarning(
            SOURCE_LOC,
            "Missing support for debug type, falling back to LLVM type",
            debugType->getName().str());
    }

    // Fallback
    if (llvmType == nullptr) {
        pallas::ErrorReporter::addError(
            SOURCE_LOC, "Fallback failed there is no LLVM type available here");
    } else {
        transformAndSetType(*llvmType, colType, dataLayout);
    }
}

void llvm2col::transformAndSetValueType(llvm::Value &value,
                                        llvm::Type *pointerType,
                                        col::Type &colType,
                                        const llvm::DataLayout &dataLayout) {
    if (auto *diType = pallas::utils::getDITypeForValue(value)) {
        if (pointerType != nullptr &&
            diType->getTag() == llvm::dwarf::DW_TAG_pointer_type) {
            col::LlvmtPointer *colPointer = colType.mutable_llvmt_pointer();
            colPointer->set_allocated_origin(generateDITypeOrigin(*diType));
            transformAndSetTypeWithDebugInfo(
                pointerType, cast<llvm::DIDerivedType>(diType)->getBaseType(),
                *colPointer->mutable_inner_type(), dataLayout);
        } else {
            transformAndSetTypeWithDebugInfo(value.getType(), diType, colType,
                                             dataLayout);
        }
    } else {
        if (pointerType == nullptr) {
            transformAndSetType(*value.getType(), colType, dataLayout);
        } else {
            transformAndSetPointerType(*pointerType, colType, dataLayout);
        }
    }
}

void llvm2col::transformAndSetType(llvm::Type &llvmType, col::Type &colType,
                                   const llvm::DataLayout &dataLayout) {
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
    case llvm::Type::HalfTyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_f16()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::BFloatTyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_b_f16()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::FloatTyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_f32()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::DoubleTyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_f64()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::X86_FP80TyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_f80()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::FP128TyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()->mutable_f128()->set_allocated_origin(
            generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
    case llvm::Type::PPC_FP128TyID: {
        col::LlvmtFloat *colFloat = colType.mutable_llvmt_float();
        colFloat->mutable_float_type()
            ->mutable_ppc_f128()
            ->set_allocated_origin(generateTypeOrigin(llvmType));
        colFloat->set_allocated_origin(generateTypeOrigin(llvmType));
        break;
    }
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
            colStruct->add_name(structType.getName().str());
        }
        colStruct->set_size_in_bits(
            dataLayout.getTypeAllocSizeInBits(&structType));
        colStruct->set_is_literal(structType.isLiteral());
        colStruct->set_packed(structType.isPacked());
        const llvm::StructLayout *structLayout =
            dataLayout.getStructLayout(&structType);
        for (size_t i = 0, end = structType.getNumElements(); i < end; ++i) {
            llvm::Type *element = structType.getElementType(i);
            col::LlvmFieldDefinition *field = colStruct->add_elements();
            field->set_offset(structLayout->getElementOffsetInBits(i));
            field->set_size(dataLayout.getTypeSizeInBits(element));
            field->set_allocated_origin(generateTypeOrigin(llvmType));
            llvm2col::transformAndSetType(*element, *field->mutable_t(),
                                          dataLayout);
        }
        break;
    }
    case llvm::Type::ArrayTyID: {
        llvm::ArrayType &arrayType = llvm::cast<llvm::ArrayType>(llvmType);
        col::LlvmtArray *colArray = colType.mutable_llvmt_array();
        colArray->set_allocated_origin(generateTypeOrigin(llvmType));
        llvm2col::transformAndSetType(*arrayType.getElementType(),
                                      *colArray->mutable_element_type(),
                                      dataLayout);
        colArray->set_num_elements(arrayType.getNumElements());
        break;
    }
    case llvm::Type::FixedVectorTyID:
    case llvm::Type::ScalableVectorTyID: {
        llvm::VectorType &vectorType = llvm::cast<llvm::VectorType>(llvmType);
        col::LlvmtVector *colVector = colType.mutable_llvmt_vector();
        colVector->set_allocated_origin(generateTypeOrigin(llvmType));
        llvm2col::transformAndSetType(*vectorType.getElementType(),
                                      *colVector->mutable_element_type(),
                                      dataLayout);
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
            llvm::cast<llvm::Constant>(llvmOperand), colExpr,
            llvmInstruction.getModule()->getDataLayout());
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
                                        col::Expr &colExpr,
                                        const llvm::DataLayout &dataLayout) {
    if (llvm::isa<llvm::ConstantAggregateZero>(llvmConstant)) {
        col::LlvmZeroedAggregateValue *colZero =
            colExpr.mutable_llvm_zeroed_aggregate_value();

        colZero->set_allocated_origin(origin);
        llvm2col::transformAndSetType(*llvmConstant.getType(),
                                      *colZero->mutable_aggregate_type(),
                                      dataLayout);
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
    case llvm::Type::HalfTyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_f16()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::BFloatTyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_b_f16()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::FloatTyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_f32()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::DoubleTyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_f64()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::X86_FP80TyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_f80()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::FP128TyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()->mutable_f128()->set_allocated_origin(
            generateTypeOrigin(*constType));
        break;
    }
    case llvm::Type::PPC_FP128TyID: {
        llvm::ConstantFP &llvmFp = llvm::cast<llvm::ConstantFP>(llvmConstant);
        col::LlvmFloatValue *floatValue = colExpr.mutable_llvm_float_value();
        floatValue->set_allocated_origin(origin);
        llvm::APInt apInt = llvmFp.getValue().bitcastToAPInt();
        transformAndSetBigInt(apInt, *floatValue->mutable_value());
        floatValue->mutable_float_type()
            ->mutable_ppc_f128()
            ->set_allocated_origin(generateTypeOrigin(*constType));
        break;
    }
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
                *colStruct->add_value(), dataLayout);
        }
        colStruct->set_allocated_origin(origin);
        llvm2col::transformAndSetType(*llvmStruct.getType(),
                                      *colStruct->mutable_struct_type(),
                                      dataLayout);

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
                    *colArray->add_value(), dataLayout);
            }
            colArray->set_allocated_origin(origin);
            llvm2col::transformAndSetType(*llvmArray.getType(),
                                          *colArray->mutable_array_type(),
                                          dataLayout);
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
                                          *colArray->mutable_array_type(),
                                          dataLayout);
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
                    *colVector->add_value(), dataLayout);
            }
            colVector->set_allocated_origin(origin);
            llvm2col::transformAndSetType(*llvmVector.getType(),
                                          *colVector->mutable_vector_type(),
                                          dataLayout);
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
                                          *colVector->mutable_vector_type(),
                                          dataLayout);
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
