#include "Util/PallasMD.h"
#include "Util/Constants.h"
#include <llvm/Support/Casting.h>

#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>

namespace pallas::utils {

std::optional<std::string> isPallasSpecLib(const llvm::Function &f) {

    auto *mdMarker = f.getMetadata(constants::PALLAS_SPEC_LIB_MARKER);
    if (mdMarker == nullptr || mdMarker->getNumOperands() != 1)
        return {};

    auto *mdTypeStr =
        llvm::dyn_cast<llvm::MDString>(mdMarker->getOperand(0).get());
    if (mdTypeStr == nullptr)
        return {};

    return mdTypeStr->getString().str();
}

bool hasPallasContract(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_FUNC_CONTRACT);
}

llvm::MDNode *getPallasContract(const llvm::Function &f) {
    if (hasPallasContract(f)) {
        return f.getMetadata(pallas::constants::PALLAS_FUNC_CONTRACT);
    }
    if (hasExternalPallasContract(f)) {
        return f.getMetadata(pallas::constants::PALLAS_EXT_CONTRACT);
    }
    return nullptr;
}

bool hasExternalPallasContract(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_EXT_CONTRACT);
}

bool hasVcllvmContract(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::METADATA_CONTRACT_KEYWORD);
}

bool isPallasExprWrapper(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_WRAPPER_FUNC);
}

bool isPallasGhostWrapper(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_GHOST_WRAPPER_FUNC);
}

bool isPallasPredDef(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_PRED_DEF);
}

std::optional<bool> isPallasPredInline(const llvm::Function &f) {
    if (!isPallasPredDef(f))
        return std::nullopt;
    auto *predDefMD = f.getMetadata(pallas::constants::PALLAS_PRED_DEF);
    if (predDefMD->getNumOperands() != 1)
        return std::nullopt;
    auto *inlineConst = llvm::dyn_cast<llvm::ConstantAsMetadata>(
        predDefMD->getOperand(0).get());
    auto *inlineVal =
        llvm::dyn_cast_if_present<llvm::ConstantInt>(inlineConst->getValue());
    if (inlineVal == nullptr || (inlineVal->getBitWidth() != 1))
        return std::nullopt;
    return std::make_optional(inlineVal->isOne());
}

bool isWellformedPallasLocation(const llvm::MDNode *mdNode) {

    if (mdNode == nullptr)
        return false;

    if (mdNode->getNumOperands() != 6)
        return false;

    // Check that first operand is a string-identifier
    if (auto *mdStr = dyn_cast<llvm::MDString>(mdNode->getOperand(0).get())) {
        if (mdStr->getString().str() != pallas::constants::PALLAS_SRC_LOC_ID)
            return false;
    } else {
        return false;
    }

    // Check that the next four operands are integer constants
    if (!isConstantInt(mdNode->getOperand(1).get()) ||
        !isConstantInt(mdNode->getOperand(2).get()) ||
        !isConstantInt(mdNode->getOperand(3).get()) ||
        !isConstantInt(mdNode->getOperand(4).get())) {
        return false;
    }

    // Check that the last operand points to a DIFile
    if (!llvm::isa<llvm::DIFile>(mdNode->getOperand(5).get())) {
        return false;
    }

    return true;
}

bool isConstantInt(llvm::Metadata *md) {
    if (auto *mdConst = dyn_cast<llvm::ConstantAsMetadata>(md)) {
        if (isa<llvm::ConstantInt>(mdConst->getValue())) {
            return true;
        }
    }
    return false;
}

llvm::Function *getWrapperFromLoopInv(const llvm::MDNode &invMD) {
    if (invMD.getNumOperands() < 2) {
        return nullptr;
    }
    auto *wFuncMD = llvm::dyn_cast_if_present<llvm::ValueAsMetadata>(
        invMD.getOperand(1).get());
    if (wFuncMD == nullptr) {
        return nullptr;
    }
    auto *wFunc =
        llvm::dyn_cast_if_present<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr || !pallas::utils::isPallasExprWrapper(*wFunc)) {
        return nullptr;
    }
    return wFunc;
}

llvm::MDNode *getPallasLoopContract(const llvm::Loop &llvmLoop) {
    // Extract the LoopID
    llvm::MDNode *loopID = llvmLoop.getLoopID();
    if (loopID == nullptr)
        return nullptr;

    for (const llvm::MDOperand &op : loopID->operands()) {
        auto *opNode = llvm::dyn_cast_if_present<llvm::MDNode>(op.get());
        // Check that the first operand is a MDString identifier for a
        // loop contract
        if (opNode != nullptr && opNode->getNumOperands() >= 2) {
            auto *idStr = llvm::dyn_cast_if_present<llvm::MDString>(
                opNode->getOperand(0).get());
            if (idStr != nullptr &&
                idStr->getString().str() ==
                    pallas::constants::PALLAS_LOOP_CONTR_ID) {
                return opNode;
            }
        }
    }
    return nullptr;
}

llvm::MDNode *getSpecStmntBlock(llvm::Instruction &instr) {
    return instr.getMetadata(pallas::constants::PALLAS_SPEC_STMNT_BLOCK);
}

} // namespace pallas::utils
