#include "IRSpec/PallasSpecDecoding.h"
#include "Util/Constants.h"
#include "Util/Exceptions.h"
#include "Util/PallasMD.h"

#include <llvm/Support/Casting.h>

namespace pallas::irspec {

const std::string SOURCE_LOC = "IRSpec::PallasSpecDecoding";

namespace {

void addError(const std::string &msg, const llvm::Metadata *md) {
    if (md == nullptr) {
        pallas::ErrorReporter::addError(SOURCE_LOC, msg);
    } else {
        pallas::ErrorReporter::addError(SOURCE_LOC, msg, md);
    }
}

void addError(const std::string &prefix, const std::string &msg,
              const llvm::Metadata *md) {
    std::string fullMsg = prefix + ": " + msg;
    addError(fullMsg, md);
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

bool decodeWArgToGhostMapping(const llvm::MDNode *md,
                              WrapperArgGhostMap &mapping,
                              const std::string &errMsg) {
    if (md == nullptr) {
        addError(errMsg, "Mapping of ghost variables may not be null.", md);
        return false;
    }

    // Decode the entries of shape {VAR_DEF, WRAPPER_VAR}
    for (const auto &op : md->operands()) {
        auto *entry = llvm::dyn_cast<llvm::MDNode>(op.get());
        if (entry == nullptr || entry->getNumOperands() != 2) {
            addError(errMsg,
                     "Mapping of ghost variables may only contain MDNodes with "
                     "exactly two operands.",
                     md);
            return false;
        }

        // Get definition of ghost-var
        auto *gVarDef =
            llvm::dyn_cast<llvm::MDNode>(entry->getOperand(0).get());
        if (!getGhostArgDef(gVarDef).has_value()) {
            addError(errMsg,
                     "First element of ghost variable mapping must point to "
                     "MDNode that defines a ghost-variable.",
                     entry);
            return false;
        }

        // Get DIVar of wrapper
        auto *wArg =
            llvm::dyn_cast<llvm::DILocalVariable>(entry->getOperand(1).get());
        if (wArg == nullptr) {
            addError(errMsg,
                     "Second element of ghost variable mapping must be "
                     "DILocalVariable.",
                     entry);
            return false;
        }

        // Add to mapping
        mapping.insert({wArg, gVarDef});
    }

    return true;
}

bool decodeWArgToVarMapping(const llvm::MDNode *md, WrapperArgVarMap &mapping,
                            const std::string &errMsg) {
    if (md == nullptr) {
        addError(errMsg, "Mapping of variables to wrapper-argsmay not be null.",
                 md);
        return false;
    }

    // Decode the entries of shape {PARENT_VAR, WRAPPER_VAR}
    for (const auto &op : md->operands()) {
        auto *entry = llvm::dyn_cast<llvm::MDNode>(op.get());
        if (entry == nullptr || entry->getNumOperands() != 2) {
            addError(errMsg,
                     "Mapping of variables to wrapper-args may only contain "
                     "MDNodes with exactly two operands.",
                     md);
            return false;
        }

        // Get DIVar of parent
        auto *pVar =
            llvm::dyn_cast<llvm::DILocalVariable>(entry->getOperand(0).get());
        if (pVar == nullptr) {
            addError(errMsg,
                     "First element of wrapper-arg mapping must be "
                     "DILocalVariable.",
                     entry);
            return false;
        }

        // Get DIVar of wrapper
        auto *wArg =
            llvm::dyn_cast<llvm::DILocalVariable>(entry->getOperand(1).get());
        if (wArg == nullptr) {
            addError(errMsg,
                     "Second element of wrapper-arg mapping must be "
                     "DILocalVariable.",
                     entry);
            return false;
        }

        // Add to mapping
        mapping.insert({wArg, pVar});
    }

    return true;
}

bool decodeWrapperArgMapping(const llvm::MDNode &md,
                             WrappedSpecElement &specElem,
                             const std::string &errMsg) {
    if (md.getNumOperands() < 6) {
        addError(errMsg, "Expected six operands", &md);
        return false;
    }

    // Given
    bool givenOk = decodeWArgToGhostMapping(
        llvm::dyn_cast<llvm::MDNode>(md.getOperand(3).get()),
        specElem.getGivenMapping(),
        errMsg + " Fourth operand of must be mapping of given-args.");
    if (!givenOk)
        return false;

    // Yields
    bool yieldsOk = decodeWArgToGhostMapping(
        llvm::dyn_cast<llvm::MDNode>(md.getOperand(4).get()),
        specElem.getYieldsMapping(),
        errMsg + "Fifth operand must be mapping of yields-args.");
    if (!yieldsOk)
        return false;

    // Regular args
    bool argsOk = decodeWArgToVarMapping(
        llvm::dyn_cast<llvm::MDNode>(md.getOperand(5).get()),
        specElem.getParentVarMapping(),
        errMsg +
            "Sixth operand must be mapping of wrapper-args to parent-vars.");

    return argsOk;
}

std::optional<ContractClause> getContractClause(llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Contract clause may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() != 6) {
        addError("Ill-formed contract clause. Expected 6 operands", md);
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
    if (wFunc == nullptr ||
        !(isPallasExprWrapper(*wFunc) || isPallasGhostWrapper(*wFunc))) {
        addError(
            "Third operand of contract clause must point to wrapper function.",
            md);
        return std::nullopt;
    }

    ContractClause clause(md, type, loc.value(), *wFunc);

    // Mapping of wrapper-args
    bool mappingOk =
        decodeWrapperArgMapping(*md, clause, "Decoding of contract clause ");
    if (!mappingOk)
        return std::nullopt;

    return std::make_optional(clause);
}

std::optional<GhostArgDef> getGhostArgDef(const llvm::MDNode *md) {

    if (md == nullptr) {
        addError("Definition of ghost argument may not be null.", md);
        return std::nullopt;
    }

    if (md->getNumOperands() != 2) {
        addError("Definition of ghost argument must have two operands.", md);
        return std::nullopt;
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

std::optional<FunctionContract> getContract(const llvm::MDNode *md) {

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
    auto pureRes = getContractPure(*md);
    if (!pureRes) {
        addError("Second operand operand of contract must be boolean constant.",
                 md);
        return std::nullopt;
    }
    bool pure = pureRes.value();

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
        auto *gDefMD = llvm::dyn_cast<llvm::MDNode>(op.get());
        if (!getGhostArgDef(gDefMD).has_value()) {
            addError("Invalid definition of given-argument.", md);
            return std::nullopt;
        }
        contract.addGivenArg(gDefMD);
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
        auto *yDefMD = llvm::dyn_cast<llvm::MDNode>(op.get());
        if (!getGhostArgDef(yDefMD).has_value()) {
            addError("Invalid definition of yields-argument.", md);
            return std::nullopt;
        }
        contract.addYieldsArg(yDefMD);
    }

    // Clauses
    unsigned int cIdx = 5;
    while (cIdx < md->getNumOperands()) {
        auto clause = getContractClause(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(cIdx).get()));
        if (!clause.has_value())
            return std::nullopt;
        // Check that number of ghost arguments is consistent between
        // contract and clause
        if (clause->getNumGiven() != contract.givenArgs.size()) {
            addError("Number of given-args does not match between clause and "
                     "contract.",
                     md);
            return std::nullopt;
        }
        if (clause->getType() != REQUIRES &&
            clause->getNumYields() != contract.yieldsArgs.size()) {
            addError("Number of yields-args does not match between clause and "
                     "contract.",
                     md);
            return std::nullopt;
        }

        if (clause->getType() == REQUIRES && clause->getNumYields() != 0) {
            addError("Number of yields-args in requires-clause must be zero",
                     md);
            return std::nullopt;
        }

        contract.addClause(clause.value());
        cIdx++;
    }

    return std::make_optional(contract);
}

std::optional<bool> getContractPure(const llvm::MDNode &md) {
    if (md.getNumOperands() < 2)
        return std::nullopt;

    auto *pureConst = asConstantInt(md.getOperand(1).get());
    if (pureConst == nullptr || (pureConst->getBitWidth() != 1))
        return std::nullopt;

    return pureConst->isOne();
}

std::optional<LoopInvariantClause> getLoopInvariantClause(llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Loop invariant clause may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() != 6) {
        addError("Ill-formed loop invariant clause. Expected six operands", md);
        return std::nullopt;
    }

    // ID-String
    auto *idStr = llvm::dyn_cast<llvm::MDString>(md->getOperand(0).get());
    if (idStr == nullptr ||
        idStr->getString().str() != pallas::constants::PALLAS_LOOP_INV_ID) {
        addError("First operand of loop invariant must be !\"pallas.loopInv\"",
                 md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError("Second operand of loop invariant must be location.", md);
        return std::nullopt;
    }

    // Wrapper function
    auto *wFuncMD =
        llvm::dyn_cast<llvm::ValueAsMetadata>(md->getOperand(2).get());
    if (wFuncMD == nullptr) {
        addError("Third operand of loop invariant must point to function.", md);
        return std::nullopt;
    }
    auto *wFunc = llvm::dyn_cast_or_null<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr ||
        !(isPallasExprWrapper(*wFunc) || isPallasGhostWrapper(*wFunc))) {
        addError("Second operand of loop invariant clause must point to "
                 "wrapper function.",
                 md);
        return std::nullopt;
    }

    LoopInvariantClause clause(md, loc.value(), *wFunc);

    // Mapping of wrapper-args
    bool mappingOk =
        decodeWrapperArgMapping(*md, clause, "Decoding of loop invariant -");
    if (!mappingOk)
        return std::nullopt;

    return std::make_optional(clause);
}

std::optional<LoopContract> getLoopContract(const llvm::MDNode *md) {
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
    } else if (typeStr == pallas::constants::PALLAS_GHOST_ASSIGN) {
        return GHOST_ASSIGN;
    }
    addError("Unknown specification-statement type.", md);
    return std::nullopt;
}

std::optional<SpecStatement> getSpecStatement(llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Specification statement may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 6) {
        addError("Ill-formed specification statement. Too few operands", md);
        return std::nullopt;
    }

    // Type
    auto type = getSpecStatementType(md->getOperand(0).get());
    if (!type.has_value()) {
        addError("Ill-formed specification statement type.", md);
        return std::nullopt;
    }

    // Check number of operands
    if (type == GHOST_ASSIGN) {
        if (md->getNumOperands() != 7) {
            addError(
                "Ill-formed specification statement. Expected seven operands",
                md);
            return std::nullopt;
        }
    } else {
        if (md->getNumOperands() != 6) {
            addError(
                "Ill-formed specification statement. Expected six operands",
                md);
            return std::nullopt;
        }
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
    if (wFunc == nullptr ||
        !(isPallasExprWrapper(*wFunc) || isPallasGhostWrapper(*wFunc))) {
        addError("Third operand of specification statement must point to "
                 "wrapper function.",
                 md);
        return std::nullopt;
    }

    SpecStatement stmnt(md, *type, *loc, *wFunc);

    // Mapping of wrapper-args
    bool mappingOk =
        decodeWrapperArgMapping(*md, stmnt, "Decoding of spec statement -");
    if (!mappingOk)
        return std::nullopt;

    // Target of ghost assign
    if (type.value() == GHOST_ASSIGN) {
        auto *aTarget = llvm::dyn_cast<llvm::MDNode>(md->getOperand(6).get());
        if (!getGhostArgDef(aTarget).has_value()) {
            addError("Seventh operand of ghost assign must point to definition "
                     "of ghost variable",
                     md);
            return std::nullopt;
        }
        stmnt.setAssignTarget(aTarget);
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

std::optional<YieldsBinding> getYieldsBinding(const llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Yields binding may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() != 4) {
        addError("Ill-formed yields binding. Too few operands", md);
        return std::nullopt;
    }

    // ID-String
    auto *idStr = llvm::dyn_cast<llvm::MDString>(md->getOperand(0).get());
    if (idStr == nullptr ||
        idStr->getString().str() != pallas::constants::PALLAS_YIELDS_BINDING) {
        addError("First operand of yields binding must be id-string", md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError("Second operand of yields binding must be location.", md);
        return std::nullopt;
    }

    // Target ghost-var from parent function
    auto *targetVarMD = llvm::dyn_cast<llvm::MDNode>(md->getOperand(2).get());
    auto targetVar = getGhostArgDef(targetVarMD);
    if (!targetVar.has_value()) {
        addError("Third operand of yields binding must point to definition of "
                 "a ghost variable.",
                 md);
        return std::nullopt;
    }

    // Yields arg from called function
    auto *yieldsArgMD = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3).get());
    auto yieldsArg = getGhostArgDef(yieldsArgMD);
    if (!targetVar.has_value()) {
        addError("Third operand of yields binding must point to definition of "
                 "a yields argument.",
                 md);
        return std::nullopt;
    }

    return std::make_optional(YieldsBinding(*loc, *targetVarMD, *yieldsArgMD));
}

std::optional<YieldsBindingBlock>
getYieldsBindingBlock(const llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Yields binding block may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 2) {
        addError("Ill-formed yields binding block. Too few operands", md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError("First operand of yields binding block must be location.", md);
        return std::nullopt;
    }

    YieldsBindingBlock yieldsBlock(loc.value());

    // Bindings
    unsigned int yIdx = 1;
    while (yIdx < md->getNumOperands()) {
        auto binding = getYieldsBinding(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(yIdx).get()));
        if (!binding.has_value())
            return std::nullopt;
        yieldsBlock.addBinding(binding.value());
        yIdx++;
    }

    return std::make_optional(yieldsBlock);
}

std::optional<GivenBinding> getGivenBinding(llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Binding to given-arg may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() != 7) {
        addError("Ill-formed binding to given-arg. Too few operands", md);
        return std::nullopt;
    }

    // ID-String
    auto *idStr = llvm::dyn_cast<llvm::MDString>(md->getOperand(0).get());
    if (idStr == nullptr ||
        idStr->getString().str() != pallas::constants::PALLAS_GIVEN_BINDING) {
        addError("First operand of given binding must be id-string", md);
        return std::nullopt;
    }

    // Loc
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(1).get()));
    if (!loc.has_value()) {
        addError(
            "Second operand of assignment ot ghost variable must be location.",
            md);
        return std::nullopt;
    }

    // Wrapper function
    auto *wFuncMD =
        llvm::dyn_cast<llvm::ValueAsMetadata>(md->getOperand(2).get());
    if (wFuncMD == nullptr) {
        addError("Third operand of assignment to ghost variable must point to "
                 "function.",
                 md);
        return std::nullopt;
    }
    auto *wFunc = llvm::dyn_cast_or_null<llvm::Function>(wFuncMD->getValue());
    if (wFunc == nullptr || !irspec::isPallasGhostWrapper(*wFunc)) {
        addError("Third operand of assignment to ghost variable must point to "
                 "ghost wrapper function.",
                 md);
        return std::nullopt;
    }

    // Given-arg def
    auto argDef = llvm::dyn_cast<llvm::MDNode>(md->getOperand(6).get());
    if (!getGhostArgDef(argDef).has_value()) {
        addError("Seventh operand of binding to given-arg must point to its "
                 "definition.",
                 md);
        return std::nullopt;
    }

    GivenBinding binding(md, *loc, *wFunc, *argDef);

    // Mapping of wrapper-args
    bool mappingOk =
        decodeWrapperArgMapping(*md, binding, "Decoding of given-binding -");
    if (!mappingOk)
        return std::nullopt;

    return std::make_optional(binding);
}

std::optional<GivenBindingBlock> getGivenBindingBlock(const llvm::MDNode *md) {
    if (md == nullptr) {
        addError("Given-binding block may not be null", md);
        return std::nullopt;
    }

    if (md->getNumOperands() < 2) {
        addError("Ill-formed given-binding block. Too few operands", md);
        return std::nullopt;
    }

    // Location
    auto loc = getSrcLoc(llvm::dyn_cast<llvm::MDNode>(md->getOperand(0).get()));
    if (!loc.has_value()) {
        addError("First operand of given-binding block must be location.", md);
        return std::nullopt;
    }

    GivenBindingBlock block(loc.value());

    // Assignments
    for (size_t idx = 1; idx < md->getNumOperands(); ++idx) {
        auto b = getGivenBinding(
            llvm::dyn_cast_or_null<llvm::MDNode>(md->getOperand(idx).get()));
        if (!b.has_value())
            return std::nullopt;
        block.addBinding(b.value());
    }

    return std::make_optional(block);
}

llvm::MDNode *getLoopContractMD(const llvm::Loop &llvmLoop) {
    // Extract the LoopID
    llvm::MDNode *loopID = llvmLoop.getLoopID();
    if (loopID == nullptr)
        return nullptr;

    return getLoopContractMD(*loopID);
}

llvm::MDNode *getLoopContractMD(llvm::MDNode &md) {
    for (const llvm::MDOperand &op : md.operands()) {
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

llvm::MDNode *getStmntBlockMD(llvm::Instruction &instr) {
    return instr.getMetadata(pallas::constants::PALLAS_SPEC_STMNT_BLOCK);
}

llvm::MDNode *getGivenBindingBlockMD(llvm::Instruction &instr) {
    return instr.getMetadata(pallas::constants::PALLAS_GIVEN_BINDING_BLOCK);
}

llvm::MDNode *getYieldsBindingBlockMD(llvm::Instruction &instr) {
    return instr.getMetadata(pallas::constants::PALLAS_YIELDS_BINDING_BLOCK);
}

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

llvm::MDNode *getContractMD(const llvm::Function &f) {
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

} // namespace pallas::irspec