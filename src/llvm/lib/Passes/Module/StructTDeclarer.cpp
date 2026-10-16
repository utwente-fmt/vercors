#include "Passes/Module/StructTDeclarer.h"
#include "Origin/OriginProvider.h"
#include "Passes/Module/RootContainer.h"
#include "Transform/Transform.h"
#include "Util/Exceptions.h"
#include "Util/PallasDIMapping.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/BinaryFormat/Dwarf.h>

namespace pallas {
const std::string SOURCE_LOC = "Passes::Module::StructTDeclarer";
using namespace llvm;

#define WARN_DI_TYPE_MISMATCH(mismatch, typeName)                              \
    ErrorReporter::addWarning(SOURCE_LOC,                                      \
                              "Debug type mismatch with LLVM type (" mismatch  \
                              "), falling back to LLVM type",                  \
                              typeName)

/*
 * StructT Declarer Result
 */

SDResult::SDResult(llvm::Module &m, col::Program &colProg)
    : m(m), colProg(colProg), internalIDMap() {}

bool SDResult::invalidate(Module &M, const PreservedAnalyses &PA,
                          ModuleAnalysisManager::Invalidator &) {
    return !PA.getChecker<RootContainer>().preservedWhenStateless();
}

std::optional<int64_t> SDResult::getStructDeclId(StructTyID typeID) {

    if (transformedDecls.find(typeID) == transformedDecls.end()) {
        // Transform the declaration
        transformedDecls.insert(typeID);
        bool ok = transformDecl(typeID);
        if (!ok) {
            transformedDecls.erase(typeID);
            return std::nullopt;
        }
    }

    return std::make_optional(getId(typeID));
}

int64_t SDResult::getId(StructTyID &typeID) {

    // Check if an ID has already been assigned to thi TypeID
    if (internalIDMap.find(typeID) != internalIDMap.end()) {
        return internalIDMap.at(typeID);
    }

    // Generate a new, unique ID for this typeID.
    // In other places we use the value of pointers as the id. In this case this
    // does not work because the id must depend on both, the llvm-type and the
    // debug-type.
    // We keep the last bit of the generated id at 1 avoid conflicts with other
    // ids (because those pointers are typically aligned, so the last bit is 0)

    int64_t new_id = nextID;
    nextID += 2;

    internalIDMap.insert({typeID, new_id});
    return new_id;
}

bool SDResult::transformDecl(StructTyID typeID) {
    // TODO: Add all used names here?
    // (I.e. if the same type is used in two places with different names)

    // Transform new StructDeclaration
    // We manually annotate this first because we need to remove the declaration
    // from the program again if the transformation fails.
    auto globalDecl = new col::GlobalDeclaration;
    auto *sDecl = globalDecl->mutable_llvm_struct_declaration();
    sDecl->set_id(getId(typeID));

    bool ok = true;
    if (typeID.second == nullptr) {
        transformSDecl(*sDecl, *typeID.first);
    } else {
        ok = transformSDeclWithDiType(*sDecl, typeID.first, *typeID.second);
    }

    if (ok) {
        // Transfer allocated declaration to col-program
        colProg.mutable_declarations()->AddAllocated(globalDecl);
    } else {
        // Clear up allocated declaration
        delete globalDecl;
    }

    return ok;
}

void SDResult::transformSDecl(col::LlvmStructDeclaration &decl,
                              llvm::Type &llvmType) {
    llvm::StructType &structType = llvm::cast<llvm::StructType>(llvmType);
    auto &dataLayout = m.getDataLayout();
    decl.set_allocated_origin(llvm2col::generateTypeOrigin(structType));
    if (!structType.isLiteral()) {
        decl.add_name(structType.getName().str());
    }
    decl.set_size_in_bits(dataLayout.getTypeAllocSizeInBits(&structType));
    decl.set_is_literal(structType.isLiteral());
    decl.set_packed(structType.isPacked());
    const auto *structLayout = dataLayout.getStructLayout(&structType);
    for (size_t i = 0, end = structType.getNumElements(); i < end; ++i) {
        llvm::Type *elemTy = structType.getElementType(i);
        auto *fieldDef = decl.add_elements();
        fieldDef->set_offset(structLayout->getElementOffsetInBits(i));
        fieldDef->set_size(dataLayout.getTypeSizeInBits(elemTy));
        fieldDef->set_allocated_origin(
            llvm2col::generateTypeOrigin(structType));
        llvm2col::transformAndSetType(*elemTy, *fieldDef->mutable_t(), *this);
    }
}

bool SDResult::transformSDeclWithDiType(col::LlvmStructDeclaration &decl,
                                        llvm::Type *llvmType,
                                        llvm::DIType &diType) {
    assert(llvm::isa<llvm::DICompositeType>(diType));
    auto &dataLayout = m.getDataLayout();
    auto &compositeDiType = llvm::cast<llvm::DICompositeType>(diType);

    decl.set_allocated_origin(llvm2col::generateDITypeOrigin(compositeDiType));
    decl.add_name(compositeDiType.getName().str());
    decl.set_size_in_bits(compositeDiType.getSizeInBits());
    decl.set_is_literal(false);
    // TODO: Fix packed vs unpacked, try to detect based on size or get rid
    // of packed as a thing entirely!
    decl.set_packed(false);

    // Collect the elements of the dbg-type that contribute to the in-memory
    // representation of the type.
    auto diElements = getDIElements(compositeDiType);

    if (llvmType == nullptr) {
        // Construct type purely from debug info.
        for (auto *member : diElements) {
            auto *fieldDef = decl.add_elements();
            fieldDef->set_offset(member->getOffsetInBits());
            fieldDef->set_size(
                pallas::utils::stripIgnored(member->getBaseType())
                    ->getSizeInBits());
            fieldDef->set_allocated_origin(
                llvm2col::generateStructMemberOrigin(*member));
            llvm2col::transformAndSetTypeWithDebugInfo(
                nullptr, member->getBaseType(), *fieldDef->mutable_t(), *this);
        }
        return true;
    }

    if (llvmType->getTypeID() != llvm::Type::StructTyID) {
        WARN_DI_TYPE_MISMATCH("struct != struct",
                              compositeDiType.getName().str());
        return false;
    }

    auto structType = cast<llvm::StructType>(llvmType);
    if (structType->hasName()) {
        decl.add_name(structType->getName().str());
    }
    std::vector<std::tuple<uint64_t, llvm::DIDerivedType *, llvm::Type *>>
        elements;
    elements.reserve(structType->getNumElements());
    const llvm::StructLayout *structLayout =
        dataLayout.getStructLayout(structType);
    for (size_t i = 0, end = structType->getNumElements(); i < end; ++i) {
        elements.push_back({structLayout->getElementOffsetInBits(i), nullptr,
                            structType->getElementType(i)});
    }

    // Match members of DI-Struct to members of LLVM-Struct
    for (auto *member : diElements) {
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
        assert(i != elements.size());
    }

    for (auto &[offset, diMember, llvmMember] : elements) {
        auto *fieldDef = decl.add_elements();
        fieldDef->set_offset(offset);
        fieldDef->set_size(dataLayout.getTypeSizeInBits(llvmMember));
        if (diMember == nullptr) {
            fieldDef->set_allocated_origin(
                llvm2col::generateTypeOrigin(*llvmMember));
        } else {
            fieldDef->set_allocated_origin(
                llvm2col::generateStructMemberOrigin(*diMember));
        }
        llvm2col::transformAndSetTypeWithDebugInfo(
            llvmMember, diMember == nullptr ? nullptr : diMember->getBaseType(),
            *fieldDef->mutable_t(), *this);
    }
    return true;
}

namespace {
bool overlapsNonEmpty(llvm::DIDerivedType &elem,
                      std::vector<llvm::DIDerivedType *> &allElements,
                      llvm::SmallSet<llvm::DIDerivedType *, 8> &emptyElements) {
    auto elemStart = elem.getOffsetInBits();
    for (auto *neElem : allElements) {
        if (emptyElements.contains(neElem)) {
            continue;
        }
        auto neStart = neElem->getOffsetInBits();
        auto neEnd =
            neStart +
            pallas::utils::stripIgnored(neElem->getBaseType())->getSizeInBits();
        if (neStart <= elemStart && elemStart < neEnd) {
            return true;
        }
    }
    return false;
}
} // namespace

std::vector<llvm::DIDerivedType *>
SDResult::getDIElements(llvm::DICompositeType &cType) const {
    std::vector<llvm::DIDerivedType *> allElements;
    collectElements(cType, allElements);

    llvm::SmallSet<llvm::DIDerivedType *, 8> emptyElements;
    for (auto *e : allElements) {
        if (isEmptyStructT(*e->getBaseType())) {
            emptyElements.insert(e);
        }
    }

    // Filter out empty elements that overlap with a non-empty element.
    // Required to account for EBO or [[no_unique_addr]] in C++.
    std::vector<llvm::DIDerivedType *> elements;
    for (auto *e : allElements) {
        // Skip empty elements that overlap with another non-empty element
        if (emptyElements.contains(e) &&
            overlapsNonEmpty(*e, allElements, emptyElements)) {
            continue;
        }
        elements.push_back(e);
    }
    llvm::stable_sort(elements,
                      [&](llvm::DIDerivedType *a, llvm::DIDerivedType *b) {
                          return a->getOffsetInBits() < b->getOffsetInBits();
                      });
    return elements;
}

void SDResult::collectElements(
    llvm::DICompositeType &cType,
    std::vector<llvm::DIDerivedType *> &elements) const {
    for (auto *element : cType.getElements()) {
        auto *dType = llvm::dyn_cast<llvm::DIDerivedType>(element);
        if (dType == nullptr ||
            (dType->getTag() != llvm::dwarf::DW_TAG_member &&
             dType->getTag() != llvm::dwarf::DW_TAG_inheritance)) {
            // Skip methods etc.
            continue;
        }

        elements.push_back(dType);
    }
}

bool SDResult::isEmptyStructT(llvm::DIType &t) const {
    auto *plainT = pallas::utils::stripIgnored(&t);
    auto *structT = llvm::dyn_cast<llvm::DICompositeType>(plainT);
    if (structT == nullptr || structT->isForwardDecl() ||
        (structT->getTag() != llvm::dwarf::DW_TAG_structure_type &&
         structT->getTag() != llvm::dwarf::DW_TAG_class_type)) {
        // Not a structure-type
        return false;
    }

    // Check if there are non-empty elements
    std::vector<llvm::DIDerivedType *> elements;
    collectElements(*structT, elements);
    for (auto *elem : elements) {
        if (!isEmptyStructT(*elem->getBaseType())) {
            return false;
        }
    }
    return true;
}

/*
 * StructTDeclarer
 */
AnalysisKey StructTDeclarer::Key;

SDResult StructTDeclarer::run(Module &M, ModuleAnalysisManager &MAM) {
    auto colProg = MAM.getResult<RootContainer>(M).program;
    SDResult result = SDResult(M, *colProg.get());
    return result;
}
} // namespace pallas
