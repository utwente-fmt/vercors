#include "IRSpec/PallasIRSpec.h"

namespace pallas::irspec {

/*
 *  WrappedSpecElement
 */

WrappedSpecElement::WrappedSpecElement(llvm::MDNode *md, const SrcLoc &loc,
                                       llvm::Function &wrapper)
    : md(md), loc(loc), wrapper(&wrapper), givenArgs(), yieldsArgs(),
      wrapperArgs() {}

llvm::MDNode *WrappedSpecElement::getMD() const { return md; }

const SrcLoc &WrappedSpecElement::getLoc() const { return loc; }

llvm::Function &WrappedSpecElement::getWrapper() const { return *wrapper; }

void WrappedSpecElement::addGivenMapping(llvm::DILocalVariable *wArg,
                                         llvm::MDNode *gDef) {
    givenArgs.insert({wArg, gDef});
}

WrapperArgGhostMap &WrappedSpecElement::getGivenMapping() { return givenArgs; }

WrapperArgGhostMap &WrappedSpecElement::getYieldsMapping() {
    return yieldsArgs;
}

WrapperArgVarMap &WrappedSpecElement::getParentVarMapping() {
    return wrapperArgs;
}

void WrappedSpecElement::addYieldsMapping(llvm::DILocalVariable *wArg,
                                          llvm::MDNode *yDef) {
    yieldsArgs.insert({wArg, yDef});
}

void WrappedSpecElement::addVarMapping(llvm::DILocalVariable *wArg,
                                       llvm::DILocalVariable *pVar) {
    wrapperArgs.insert({wArg, pVar});
}

llvm::MDNode *
WrappedSpecElement::getGivenDef(llvm::DILocalVariable *wArg) const {
    auto res = givenArgs.find(wArg);
    if (res != givenArgs.end()) {
        return res->getSecond();
    }
    return nullptr;
}

llvm::MDNode *
WrappedSpecElement::getYieldsDef(llvm::DILocalVariable *wArg) const {
    auto res = yieldsArgs.find(wArg);
    if (res != yieldsArgs.end()) {
        return res->getSecond();
    }
    return nullptr;
}

llvm::DILocalVariable *
WrappedSpecElement::getParentVar(llvm::DILocalVariable *wArg) const {
    auto res = wrapperArgs.find(wArg);
    if (res != wrapperArgs.end()) {
        return res->getSecond();
    }
    return nullptr;
}

llvm::MDNode *
WrappedSpecElement::getGhostDef(llvm::DILocalVariable *wArg) const {
    if (auto *gDef = getGivenDef(wArg))
        return gDef;
    return getYieldsDef(wArg);
}

llvm::DILocalVariable *
WrappedSpecElement::getVarForGhostDef(const llvm::MDNode &gDef) const {
    for (auto [v, defMD] : givenArgs)
        if (defMD == &gDef)
            return v;
    for (auto [v, defMD] : yieldsArgs)
        if (defMD == &gDef)
            return v;
    return nullptr;
}

unsigned WrappedSpecElement::getNumGiven() const { return givenArgs.size(); }

unsigned WrappedSpecElement::getNumYields() const { return yieldsArgs.size(); }

unsigned WrappedSpecElement::getNumGhostArgs() const {
    return getNumGiven() + getNumYields();
}

/*
 *  ContractClause
 */

ContractClause::ContractClause(llvm::MDNode *md, const ContractClauseType &type,
                               const SrcLoc &loc,
                               llvm::Function &wrapperFunction)
    : WrappedSpecElement(md, loc, wrapperFunction), type(type) {}

ContractClauseType ContractClause::getType() const { return type; }

/*
 *  LoopInvariantClause
 */

LoopInvariantClause::LoopInvariantClause(llvm::MDNode *md, const SrcLoc &loc,
                                         llvm::Function &wrapperFunction)
    : WrappedSpecElement(md, loc, wrapperFunction) {}

/*
 *  SpecStatement
 */

SpecStatement::SpecStatement(llvm::MDNode *md, const SpecStatementType type,
                             const SrcLoc &loc, llvm::Function &wrapperFunction)
    : WrappedSpecElement(md, loc, wrapperFunction), type(type) {}

SpecStatementType SpecStatement::getType() const { return type; }

void SpecStatement::setAssignTarget(llvm::MDNode *target) {
    assignTarget = target;
}

llvm::MDNode *SpecStatement::getAssignTarget() const { return assignTarget; }

/*
 *  GivenBinding
 */
GivenBinding::GivenBinding(llvm::MDNode *md, const SrcLoc &loc,
                           llvm::Function &wrapper, llvm::MDNode &givenDef)
    : WrappedSpecElement(md, loc, wrapper), givenDef(&givenDef) {}

llvm::MDNode *GivenBinding::getGivenDef() const { return givenDef; }

/*
 *  GivenBinding
 */
YieldsBinding::YieldsBinding(const SrcLoc &loc, llvm::MDNode &targetVar,
                             llvm::MDNode &yieldsArg)
    : loc(loc), targetVar(&targetVar), yieldsArg(&yieldsArg) {}

const SrcLoc &YieldsBinding::getLoc() const { return loc; }

llvm::MDNode &YieldsBinding::getTargetVar() const { return *targetVar; }

llvm::MDNode &YieldsBinding::getYieldsArg() const { return *yieldsArg; }

} // namespace pallas::irspec