#ifndef PALLAS_IRSPEC_H
#define PALLAS_IRSPEC_H

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Metadata.h>

/**
 * Representations to decode the Pallas specifications into.
 */
namespace pallas::irspec {

/**
 * Representation of source-locations as encoded in the specification format of
 * Pallas.
 */
struct SrcLoc {
    unsigned int startLine;
    unsigned int startCol;
    unsigned int endLine;
    unsigned int endCol;
    llvm::DIFile *file;

    SrcLoc(unsigned int startLine, unsigned int startCol, unsigned int endLine,
           unsigned int endCol, llvm::DIFile *file)
        : startLine(startLine), startCol(startCol), endLine(endLine),
          endCol(endCol), file(file) {}
};

/**
 * Types of contract clauses in the specification format of Pallas.
 */
enum ContractClauseType { REQUIRES, ENSURES };

/**
 * Maps DILocalVariables from the wrapper-function to DILocalVariables of the
 * function where the specification is used (parentFunction);
 */
typedef llvm::SmallDenseMap<llvm::DILocalVariable *, llvm::DILocalVariable *, 8>
    WrapperArgVarMap;

/**
 * Maps DILocalVariables from the wrapper-function to metadata nodes that define
 * ghost variable in the function where the specification is used
 * (parentFunction);
 */
typedef llvm::SmallDenseMap<llvm::DILocalVariable *, llvm::MDNode *, 4>
    WrapperArgGhostMap;

/**
 * Base-class for wrapped specification-elements.
 * Contains:
 * - A source location
 * - Pointer to a wrapper function
 * - Mappings of the wrapper function's arguments to given- and yields-arguments
 * and regular variable sof the parent function.
 */
class WrappedSpecElement {
  public:
    WrappedSpecElement(llvm::MDNode *md, const SrcLoc &loc,
                       llvm::Function &wrapper);

    virtual ~WrappedSpecElement() = default;

    llvm::MDNode *getMD() const;

    const SrcLoc &getLoc() const;

    llvm::Function &getWrapper() const;

    WrapperArgGhostMap &getGivenMapping();

    WrapperArgGhostMap &getYieldsMapping();

    WrapperArgVarMap &getParentVarMapping();

    void addGivenMapping(llvm::DILocalVariable *wArg, llvm::MDNode *gDef);

    void addYieldsMapping(llvm::DILocalVariable *wArg, llvm::MDNode *yDef);

    void addVarMapping(llvm::DILocalVariable *wArg,
                       llvm::DILocalVariable *pVar);

    llvm::MDNode *getGivenDef(llvm::DILocalVariable *wArg) const;

    llvm::MDNode *getYieldsDef(llvm::DILocalVariable *wArg) const;

    llvm::MDNode *getGhostDef(llvm::DILocalVariable *wArg) const;

    llvm::DILocalVariable *getVarForGhostDef(const llvm::MDNode &gDef) const;

    llvm::DILocalVariable *getParentVar(llvm::DILocalVariable *wArg) const;

    unsigned getNumGiven() const;

    unsigned getNumYields() const;

    unsigned getNumGhostArgs() const;

  protected:
    llvm::MDNode *md;
    SrcLoc loc;
    llvm::Function *wrapper;
    WrapperArgGhostMap givenArgs;
    WrapperArgGhostMap yieldsArgs;
    WrapperArgVarMap wrapperArgs;
};

/**
 * Representation of a contract clause as used in the specification format of
 * Pallas.
 */
class ContractClause : public WrappedSpecElement {
  public:
    ContractClause(llvm::MDNode *md, const ContractClauseType &type,
                   const SrcLoc &loc, llvm::Function &wrapperFunction);

    ContractClauseType getType() const;

  protected:
    ContractClauseType type;
};

/**
 * Representation of the definition of a ghost argument in the specification
 * format of Pallas.
 */
struct GhostArgDef {
    SrcLoc loc;
    std::string name;

    GhostArgDef(const SrcLoc &loc, const std::string &name)
        : loc(loc), name(name) {}
};

/**
 * Representation of a function contract in the specification format of
 * Pallas.
 */
struct FunctionContract {
    SrcLoc loc;
    bool pure;
    bool assumed;
    llvm::SmallVector<llvm::MDNode *, 4> givenArgs;
    llvm::SmallVector<llvm::MDNode *, 4> yieldsArgs;
    llvm::SmallVector<ContractClause, 4> clauses;

    FunctionContract(const SrcLoc &loc, const bool pure, const bool assumed)
        : loc(loc), pure(pure), assumed(assumed) {}

    void addClause(ContractClause clause) { clauses.push_back(clause); }

    void addGivenArg(llvm::MDNode *arg) { givenArgs.push_back(arg); }

    void addYieldsArg(llvm::MDNode *arg) { yieldsArgs.push_back(arg); }
};

/**
 * Representation of a clause that is part of a block of loop invariants.
 */
class LoopInvariantClause : public WrappedSpecElement {
  public:
    LoopInvariantClause(llvm::MDNode *md, const SrcLoc &loc,
                        llvm::Function &wrapperFunction);
};

/**
 * Representation of a block of loop invariants in the specification format of
 * Pallas.
 */
struct LoopContract {
    SrcLoc loc;
    llvm::SmallVector<LoopInvariantClause, 4> clauses;

    LoopContract(const SrcLoc &loc) : loc(loc) {}

    void addClause(LoopInvariantClause clause) { clauses.push_back(clause); }
};

/**
 * Types of specification statements in the specification format of Pallas.
 */
enum SpecStatementType { ASSERT, ASSUME, FOLD, UNFOLD, GHOST_ASSIGN };

/**
 * Representation of a specification statement in the specification-format
 * of Pallas.
 */
struct SpecStatement : public WrappedSpecElement {
  public:
    SpecStatement(llvm::MDNode *md, const SpecStatementType type,
                  const SrcLoc &loc, llvm::Function &wrapperFunction);

    SpecStatementType getType() const;

    void setAssignTarget(llvm::MDNode *target);

    // TODO: Turn this into a separate subclass!
    llvm::MDNode *getAssignTarget() const;

  protected:
    SpecStatementType type;

    // If the statement is a ghost-assign, this is the MD-node that encodes the
    // target variable.
    llvm::MDNode *assignTarget = nullptr;
};

/**
 * Representation of a block of specification statements in the specification
 * format of Pallas.
 */
struct SpecStatementBlock {
    SrcLoc loc;
    llvm::SmallVector<SpecStatement, 4> statements;

    SpecStatementBlock(const SrcLoc &loc) : loc(loc) {}

    void addStatement(SpecStatement stmnt) { statements.push_back(stmnt); }
};

/**
 * Binding of the value returned with a yields-argument to another
 * ghost variable.
 */
class YieldsBinding {
    SrcLoc loc;
    llvm::MDNode *targetVar;
    llvm::MDNode *yieldsArg;

  public:
    YieldsBinding(const SrcLoc &loc, llvm::MDNode &targetVar,
                  llvm::MDNode &yieldsArg);

    const SrcLoc &getLoc() const;

    llvm::MDNode &getTargetVar() const;

    llvm::MDNode &getYieldsArg() const;
};

/**
 * Block of yields-argument bindings.
 */
struct YieldsBindingBlock {
    SrcLoc loc;
    llvm::SmallVector<YieldsBinding, 2> bindings;

    YieldsBindingBlock(const SrcLoc &loc) : loc(loc) {}

    void addBinding(YieldsBinding binding) { bindings.push_back(binding); }
};

/**
 * Binding of a value to a given-variable.
 */
class GivenBinding : public WrappedSpecElement {
  public:
    GivenBinding(llvm::MDNode *md, const SrcLoc &loc, llvm::Function &wrapper,
                 llvm::MDNode &givenDef);

    llvm::MDNode *getGivenDef() const;

  protected:
    llvm::MDNode *givenDef;
};

/**
 * Block of bindings to given-arguments.
 * Also used for given bindings.
 */
struct GivenBindingBlock {
    SrcLoc loc;
    llvm::SmallVector<GivenBinding, 4> bindings;

    GivenBindingBlock(const SrcLoc &loc) : loc(loc) {}

    void addBinding(GivenBinding binding) { bindings.push_back(binding); }
};

} // namespace pallas::irspec

#endif // PALLAS_IRSPEC_H
