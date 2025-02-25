#include "Util/PallasMD.h"
#include "Util/Constants.h"
#include <llvm/Support/Casting.h>

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

bool hasVcllvmContract(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::METADATA_CONTRACT_KEYWORD);
}

bool isPallasExprWrapper(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::PALLAS_WRAPPER_FUNC);
}

bool isWellformedPallasLocation(const llvm::MDNode *mdNode) {

    if (mdNode == nullptr)
        return false;

    if (mdNode->getNumOperands() != 5)
        return false;

    // Check that first operand is a string-identifier
    if (auto *mdStr = dyn_cast<llvm::MDString>(mdNode->getOperand(0).get())) {
        if (mdStr->getString().str() != pallas::constants::PALLAS_SRC_LOC_ID)
            return false;
    } else {
        return false;
    }

    // Check that the last four operands are integer constants
    if (!isConstantInt(mdNode->getOperand(1).get()) ||
        !isConstantInt(mdNode->getOperand(2).get()) ||
        !isConstantInt(mdNode->getOperand(3).get()) ||
        !isConstantInt(mdNode->getOperand(4).get())) {
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
} // namespace pallas::utils
