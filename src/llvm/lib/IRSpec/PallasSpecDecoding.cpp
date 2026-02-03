#include "IRSpec/PallasSpecDecoding.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"

#include <llvm/Support/Casting.h>

namespace pallas::irspec {

const std::string SOURCE_LOC = "IRSpec::PallasSpecDecoding";

namespace {

void addError(std::string msg, const llvm::Metadata *md) {
    if (md == nullptr) {
        pallas::ErrorReporter::addError(SOURCE_LOC, msg);
    } else {
        pallas::ErrorReporter::addError(SOURCE_LOC, msg, md);
    }
}

} // namespace

bool isWellformedPallasLocation(const llvm::MDNode *mdNode) {

    if (mdNode == nullptr)
        return false;

    if (mdNode->getNumOperands() != 6)
        return false;

    // First operand should be identifier-string
    auto *mdStr = llvm::dyn_cast<llvm::MDString>(mdNode->getOperand(0).get());
    if (mdStr == nullptr ||
        mdStr->getString().str() != pallas::constants::PALLAS_SRC_LOC_ID) {
        return false;
    }

    // Next four operands should be integer constants
    if (asConstantInt(mdNode->getOperand(1).get()) == nullptr ||
        asConstantInt(mdNode->getOperand(2).get()) == nullptr ||
        asConstantInt(mdNode->getOperand(3).get()) == nullptr ||
        asConstantInt(mdNode->getOperand(4).get()) == nullptr) {
        return false;
    }

    // Last operand should point to DIFile
    if (!llvm::isa<llvm::DIFile>(mdNode->getOperand(5).get()))
        return false;

    return true;
}

std::optional<irspec::SrcLoc> getSrcLoc(const llvm::MDNode *md) {
    if (!isWellformedPallasLocation(md)) {
        addError("Ill-formed Pallas source location.", md);
        return std::nullopt;
    }

    return irspec::SrcLoc(
        asConstantInt(md->getOperand(1).get())->getSExtValue(),
        asConstantInt(md->getOperand(2).get())->getSExtValue(),
        asConstantInt(md->getOperand(3).get())->getSExtValue(),
        asConstantInt(md->getOperand(4).get())->getSExtValue(),
        llvm::dyn_cast<llvm::DIFile>(md->getOperand(5)));
}

const llvm::ConstantInt *asConstantInt(const llvm::Metadata *md) {
    auto *mdConst = llvm::dyn_cast_if_present<llvm::ConstantAsMetadata>(md);
    if (mdConst == nullptr)
        return nullptr;
    return llvm::dyn_cast<llvm::ConstantInt>(mdConst->getValue());
}

std::optional<ContractClause> getContractClause(const llvm::MDNode *md,
                                                bool hasImplicitArgs) {
    if (md == nullptr) {
        addError("Contract clause may not be null", md);
        return std::nullopt;
    }

    if ((!hasImplicitArgs && md->getNumOperands() < 5) ||
        (hasImplicitArgs && md->getNumOperands() != 5)) {
        addError("Ill-formed contract clause. Too few operands", md);
        return std::nullopt;
    }

    // Identifier
    auto *typeMD = llvm::dyn_cast<llvm::MDString>(md->getOperand(0).get());
    if (typeMD == nullptr) {
        addError("First operand of contract clause should be a string.", md);
        return std::nullopt;
    }
    auto typeStr = typeMD->getString().str();
    ContractClauseType type;
    if (typeStr == pallas::constants::PALLAS_REQUIRES)
        type = REQUIRES;
    else if (typeStr == pallas::constants::PALLAS_ENSURES)
        type = ENSURES;
    else {
        addError("Unknown type of contract clause.", md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError("Second operand of contract clause must be location.", md);
        return std::nullopt;
    }

    // Wrapper function
    auto *wFuncMD =
        llvm::dyn_cast<llvm::ValueAsMetadata>(md->getOperand(2).get());
    if (wFuncMD == nullptr) {
        addError("Third operand of contract clause must point to function.",
                 md);
        return std::nullopt;
    }
    auto *wFunc = llvm::dyn_cast_or_null<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr || !utils::isPallasExprWrapper(*wFunc)) {
        addError(
            "Third operand of contract clause must point to wrapper function.",
            md);
        return std::nullopt;
    }

    ContractClause clause(type, loc.value(), wFunc);

    // Given
    auto *givenList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3).get());
    if (givenList == nullptr) {
        addError("Fourth operand of contract clause must point to list of "
                 "DIVariables.",
                 md);
        return std::nullopt;
    }
    for (const auto &g : givenList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(g.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of given-args.",
                     givenList);
            return std::nullopt;
        }
        clause.addGivenArg(var);
    }

    // Yields
    auto *yieldsList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(4).get());
    if (yieldsList == nullptr) {
        addError("Fifth operand of contract clause must point to list of "
                 "DIVariables.",
                 md);
        return std::nullopt;
    }
    for (const auto &y : yieldsList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(y.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of yields-args.",
                     yieldsList);
            return std::nullopt;
        }
        clause.addYieldsArg(var);
    }

    // DIVariables
    unsigned int vIdx = 5;
    while (vIdx < md->getNumOperands()) {
        auto *diVar =
            llvm::dyn_cast<llvm::DILocalVariable>(md->getOperand(vIdx).get());
        if (diVar == nullptr) {
            addError("Expected DIVariable at index " + std::to_string(vIdx) +
                         " of contract clause.",
                     md);
            return std::nullopt;
        }
        clause.addWrapperArg(diVar);
        vIdx++;
    }

    return std::make_optional(clause);
}

std::optional<GhostArgDef> getGhostArgDef(const llvm::MDNode *md) {
    if (md == nullptr || md->getNumOperands() != 2) {
        addError("Definition of ghost argument must have two operands.", md);
    }

    // Loc
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError("First operand of ghost argument defintion must be location.",
                 md);
        return std::nullopt;
    }

    // Name
    auto nameMD = llvm::dyn_cast<llvm::MDString>(md->getOperand(1).get());
    if (nameMD == nullptr) {
        addError("Second operand of ghost argument defintion must be MDString.",
                 md);
        return std::nullopt;
    }

    auto argDef = GhostArgDef(loc.value(), nameMD->getString().str());
    return std::make_optional(argDef);
}

std::optional<FunctionContract> getContract(const llvm::MDNode *md,
                                            bool externalOrGhost) {

    if (md == nullptr) {
        addError("Contract may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 5) {
        addError("Ill-formed contract. Too few operands", md);
        return std::nullopt;
    }

    // location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError("First operand of contract must be location.", md);
        return std::nullopt;
    }

    // Pure
    auto *pureConst = asConstantInt(md->getOperand(1).get());
    if (pureConst == nullptr || (pureConst->getBitWidth() != 1)) {
        addError("Second operand operand of contract must be boolean constant.",
                 md);
        return std::nullopt;
    }
    bool pure = pureConst->isOne();

    // Assumed
    auto *assumedConst = asConstantInt(md->getOperand(2).get());
    if (assumedConst == nullptr || (assumedConst->getBitWidth() != 1)) {
        addError("Third operand operand of contract must be boolean constant.",
                 md);
        return std::nullopt;
    }
    bool assumed = assumedConst->isOne();

    FunctionContract contract(loc.value(), pure, assumed);

    // Given
    auto *givenList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3).get());
    if (givenList == nullptr) {
        addError("Fourth operand of contract must point to list ghost "
                 "argument definitions.",
                 md);
        return std::nullopt;
    }
    for (const auto &op : givenList->operands()) {
        auto gDef = getGhostArgDef(llvm::dyn_cast<llvm::MDNode>(op.get()));
        if (!gDef.has_value())
            return std::nullopt;
        contract.addGivenArg(*gDef);
    }

    // Yields
    auto yieldsList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(4).get());
    if (yieldsList == nullptr) {
        addError("Fifth operand of contract must point to list ghost "
                 "argument definitions.",
                 md);
        return std::nullopt;
    }
    for (const auto &op : yieldsList->operands()) {
        auto yDef = getGhostArgDef(llvm::dyn_cast<llvm::MDNode>(op.get()));
        if (!yDef.has_value())
            return std::nullopt;
        contract.addYieldsArg(*yDef);
    }

    // Clauses
    unsigned int cIdx = 5;
    while (cIdx < md->getNumOperands()) {
        auto clause = getContractClause(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(cIdx).get()),
            externalOrGhost);
        if (!clause.has_value())
            return std::nullopt;
        // Check that number of ghost arguments is consistent between
        // contract and clause
        if (clause->givenArgs.size() != contract.givenArgs.size()) {
            addError("Number of given-args does not match between clause and "
                     "contract.",
                     md);
            return std::nullopt;
        }
        if (clause->type != REQUIRES &&
            clause->yieldsArgs.size() != contract.yieldsArgs.size()) {
            addError("Number of yields-args does not match between clause and "
                     "contract.",
                     md);
            return std::nullopt;
        }

        if (clause->type == REQUIRES && clause->yieldsArgs.size() != 0) {
            addError("Number of yields-args in requires-clause must be zero",
                     md);
            return std::nullopt;
        }

        contract.addClause(clause.value());
        cIdx++;
    }

    return std::make_optional(contract);
}

std::optional<LoopInvariantClause>
getLoopInvariantClause(const llvm::MDNode *md) {
    // TODO: De-duplicate his with the decoding of the contract clauses
    if (md == nullptr) {
        addError("Loop invariant clause may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 4) {
        addError("Ill-formed loop invariant  clause. Too few operands", md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError("First operand of loop invariant clause must be location.",
                 md);
        return std::nullopt;
    }

    // Wrapper function
    auto *wFuncMD =
        llvm::dyn_cast<llvm::ValueAsMetadata>(md->getOperand(1).get());
    if (wFuncMD == nullptr) {
        addError("Second operand of contract clause must point to function.",
                 md);
        return std::nullopt;
    }
    auto *wFunc = llvm::dyn_cast_or_null<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr || !utils::isPallasExprWrapper(*wFunc)) {
        addError("Second operand of loop invariant clause must point to "
                 "wrapper function.",
                 md);
        return std::nullopt;
    }

    LoopInvariantClause clause(loc.value(), wFunc);

    // Given
    auto *givenList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(2).get());
    if (givenList == nullptr) {
        addError("Third operand of loop invariant clause must point to list of "
                 "DIVariables.",
                 md);
        return std::nullopt;
    }
    for (const auto &g : givenList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(g.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of given-args.",
                     givenList);
            return std::nullopt;
        }
        clause.addGivenArg(var);
    }

    // Yields
    auto *yieldsList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3).get());
    if (yieldsList == nullptr) {
        addError(
            "Fourth operand of loop invariant clause must point to list of "
            "DIVariables.",
            md);
        return std::nullopt;
    }
    for (const auto &y : yieldsList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(y.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of yields-args.",
                     yieldsList);
            return std::nullopt;
        }
        clause.addYieldsArg(var);
    }

    // DIVariables
    unsigned int vIdx = 4;
    while (vIdx < md->getNumOperands()) {
        auto *diVar =
            llvm::dyn_cast<llvm::DILocalVariable>(md->getOperand(vIdx).get());
        if (diVar == nullptr) {
            addError("Expected DIVariable at index " + std::to_string(vIdx) +
                         " of loop invariant clause.",
                     md);
            return std::nullopt;
        }
        clause.addWrapperArg(diVar);
        vIdx++;
    }

    return std::make_optional(clause);
}

std::optional<LoopContract> getLoopContract(const llvm::MDNode *md) {
    // TODO: De-duplicate this with the function contracts
    if (md == nullptr) {
        addError("Loop invariant may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 3) {
        addError("Ill-formed loop invariant. Too few operands", md);
        return std::nullopt;
    }

    // Identifier
    auto *idStr = llvm::dyn_cast<llvm::MDString>(md->getOperand(0).get());
    if (idStr == nullptr ||
        idStr->getString().str() != pallas::constants::PALLAS_LOOP_CONTR_ID) {
        addError("First operand of loop contract must be identifier string.",
                 md);
        return std::nullopt;
    }

    // location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError("Second operand of loop contract must be location.", md);
        return std::nullopt;
    }

    LoopContract loopInv(loc.value());

    // Clauses
    unsigned int cIdx = 2;
    while (cIdx < md->getNumOperands()) {
        auto clause = getLoopInvariantClause(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(cIdx).get()));
        if (!clause.has_value())
            return std::nullopt;
        loopInv.addClause(clause.value());
        cIdx++;
    }

    return std::make_optional(loopInv);
}

std::optional<SpecStatementType>
getSpecStatementType(const llvm::Metadata *md) {
    auto mdStr = llvm::dyn_cast_if_present<llvm::MDString>(md);
    if (mdStr == nullptr) {
        addError("Type of specification statement must be MDString", md);
        return std::nullopt;
    }

    auto typeStr = mdStr->getString().str();
    if (typeStr == pallas::constants::PALLAS_ASSERT) {
        return ASSERT;
    } else if (typeStr == pallas::constants::PALLAS_ASSUME) {
        return ASSUME;
    } else if (typeStr == pallas::constants::PALLAS_FOLD) {
        return FOLD;
    } else if (typeStr == pallas::constants::PALLAS_UNFOLD) {
        return UNFOLD;
    }
    addError("Unknown specification-statement type.", md);
    return std::nullopt;
}

std::optional<SpecStatement> getSpecStatement(const llvm::MDNode *md) {
    // TODO: De-duplicate this with the other specification constructs
    // Check number of operands
    if (md == nullptr) {
        addError("Specification statement may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 5) {
        addError("Ill-formed specification statement. Too few operands", md);
        return std::nullopt;
    }

    // Type
    auto type = getSpecStatementType(md->getOperand(0).get());
    if (!type.has_value()) {
        addError("Ill-formed specification statement type.", md);
        return std::nullopt;
    }

    // Loc
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError("Second operand of specification statement must be location.",
                 md);
        return std::nullopt;
    }

    // Wrapper function
    auto *wFuncMD =
        llvm::dyn_cast<llvm::ValueAsMetadata>(md->getOperand(2).get());
    if (wFuncMD == nullptr) {
        addError(
            "Third operand of specification statement must point to function.",
            md);
        return std::nullopt;
    }
    auto *wFunc = llvm::dyn_cast_or_null<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr || !utils::isPallasExprWrapper(*wFunc)) {
        addError("Third operand of specification statement must point to "
                 "wrapper function.",
                 md);
        return std::nullopt;
    }

    SpecStatement stmnt(*type, *loc, wFunc);

    // Given-args
    auto *givenList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3).get());
    if (givenList == nullptr) {
        addError(
            "Fourth operand of specification statement must point to list of "
            "DIVariables.",
            md);
        return std::nullopt;
    }
    for (const auto &g : givenList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(g.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of given-args.",
                     givenList);
            return std::nullopt;
        }
        stmnt.addGivenArg(var);
    }

    // Yields-args
    auto *yieldsList = llvm::dyn_cast<llvm::MDNode>(md->getOperand(4).get());
    if (yieldsList == nullptr) {
        addError("Fifth operand of specification statement clause must point "
                 "to list of DIVariables.",
                 md);
        return std::nullopt;
    }
    for (const auto &y : yieldsList->operands()) {
        auto *var = llvm::dyn_cast<llvm::DILocalVariable>(y.get());
        if (var == nullptr) {
            addError("Expected DILocalVariable in list of yields-args.",
                     yieldsList);
            return std::nullopt;
        }
        stmnt.addYieldsArg(var);
    }

    // Wrapper args
    unsigned int vIdx = 5;
    while (vIdx < md->getNumOperands()) {
        auto *diVar =
            llvm::dyn_cast<llvm::DILocalVariable>(md->getOperand(vIdx).get());
        if (diVar == nullptr) {
            addError("Expected DIVariable at index " + std::to_string(vIdx) +
                         " of specification statement.",
                     md);
            return std::nullopt;
        }
        stmnt.addWrapperArg(diVar);
        vIdx++;
    }

    return std::make_optional(stmnt);
}

std::optional<SpecStatementBlock>
getSpecStatementBlock(const llvm::MDNode *md) {

    if (md == nullptr) {
        addError("Specification statement-block may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 2) {
        addError("Ill-formed specification statement block. Too few operands",
                 md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError(
            "First operand of specification statement block must be location.",
            md);
        return std::nullopt;
    }

    SpecStatementBlock stmntBlock(loc.value());

    // Statements
    unsigned int cIdx = 1;
    while (cIdx < md->getNumOperands()) {
        auto stmnt = getSpecStatement(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(cIdx).get()));
        if (!stmnt.has_value())
            return std::nullopt;
        stmntBlock.addStatement(stmnt.value());
        cIdx++;
    }

    return std::make_optional(stmntBlock);
}

} // namespace pallas::irspec