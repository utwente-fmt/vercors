#ifndef PALLAS_IRSPEC_H
#define PALLAS_IRSPEC_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DebugInfoMetadata.h>

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
 * Representation of a contract clause as used in the specification format of
 * Pallas.
 */
struct ContractClause {
    ContractClauseType type;
    SrcLoc loc;
    llvm::Function *wrapperFunction;
    llvm::SmallVector<llvm::DILocalVariable *> givenArgs;
    llvm::SmallVector<llvm::DILocalVariable *> yieldsArgs;
    llvm::SmallVector<llvm::DILocalVariable *> wrapperArgs;

    ContractClause(const ContractClauseType &type, const SrcLoc &loc,
                   llvm::Function *wrapperFunction)
        : type(type), loc(loc), wrapperFunction(wrapperFunction) {}

    void addWrapperArg(llvm::DILocalVariable *v) { wrapperArgs.push_back(v); }

    void addGivenArg(llvm::DILocalVariable *v) { givenArgs.push_back(v); }

    void addYieldsArg(llvm::DILocalVariable *v) { yieldsArgs.push_back(v); }
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
    llvm::SmallVector<GhostArgDef> givenArgs;
    llvm::SmallVector<GhostArgDef> yieldsArgs;
    llvm::SmallVector<ContractClause> clauses;

    FunctionContract(const SrcLoc &loc, const bool pure, const bool assumed)
        : loc(loc), pure(pure), assumed(assumed) {}

    void addClause(ContractClause clause) { clauses.push_back(clause); }

    void addGivenArg(GhostArgDef arg) { givenArgs.push_back(arg); }

    void addYieldsArg(GhostArgDef arg) { yieldsArgs.push_back(arg); }
};

/**
 * Representation of a clause that is part of a block of loop invariants.
 */
struct LoopInvariantClause {
    SrcLoc loc;
    llvm::Function *wrapperFunction;
    llvm::SmallVector<llvm::DILocalVariable *> givenArgs;
    llvm::SmallVector<llvm::DILocalVariable *> yieldsArgs;
    llvm::SmallVector<llvm::DILocalVariable *> wrapperArgs;

    LoopInvariantClause(const SrcLoc &loc, llvm::Function *wrapperFunction)
        : loc(loc), wrapperFunction(wrapperFunction) {}

    void addWrapperArg(llvm::DILocalVariable *v) { wrapperArgs.push_back(v); }

    void addGivenArg(llvm::DILocalVariable *v) { givenArgs.push_back(v); }

    void addYieldsArg(llvm::DILocalVariable *v) { yieldsArgs.push_back(v); }
};

/**
 * Representation of a block of loop invariants in the specification format of
 * Pallas.
 */
struct LoopContract {
    SrcLoc loc;
    llvm::SmallVector<LoopInvariantClause> clauses;

    LoopContract(const SrcLoc &loc) : loc(loc) {}

    void addClause(LoopInvariantClause clause) { clauses.push_back(clause); }
};

/**
 * Types of specification statements in the specification format of Pallas.
 */
enum SpecStatementType { ASSERT, ASSUME, FOLD, UNFOLD };

/**
 * Representation of a specification statement in the specification-format
 * of Pallas.
 */
// TODO: De-duplicate this with the other specification constructs.
// (I.e. make base-class for specification clauses and blocks)
struct SpecStatement {
    SpecStatementType type;
    SrcLoc loc;
    llvm::Function *wrapperFunction;
    llvm::SmallVector<llvm::DILocalVariable *> givenArgs;
    llvm::SmallVector<llvm::DILocalVariable *> yieldsArgs;
    llvm::SmallVector<llvm::DILocalVariable *> wrapperArgs;

    SpecStatement(const SpecStatementType &type, const SrcLoc &loc,
                  llvm::Function *wrapperFunction)
        : type(type), loc(loc), wrapperFunction(wrapperFunction) {}

    void addWrapperArg(llvm::DILocalVariable *v) { wrapperArgs.push_back(v); }

    void addGivenArg(llvm::DILocalVariable *v) { givenArgs.push_back(v); }

    void addYieldsArg(llvm::DILocalVariable *v) { yieldsArgs.push_back(v); }
};

/**
 * Representation of a block of specification statements in the specification
 * format of Pallas.
 */
struct SpecStatementBlock {
    SrcLoc loc;
    llvm::SmallVector<SpecStatement> statements;

    SpecStatementBlock(const SrcLoc &loc) : loc(loc) {}

    void addStatement(SpecStatement stmnt) { statements.push_back(stmnt); }
};

/**
 * Binding of the value returned with a yields-argument to another
 * ghost variable.
 */
struct YieldsBinding {
    SrcLoc loc;
    std::string targetVarName;
    std::string yieldsArgName;

    YieldsBinding(const SrcLoc &loc, const std::string &targetVarName,
                  const std::string &yieldsArgName)
        : loc(loc), targetVarName(targetVarName), yieldsArgName(yieldsArgName) {
    }
};

/**
 * Block of yields-argument bindings.
 */
struct YieldsBindingBlock {
    SrcLoc loc;
    llvm::SmallVector<YieldsBinding, 2> bindings;

    YieldsBindingBlock(const SrcLoc &loc);

    void addBinding(YieldsBinding binding) { bindings.push_back(binding); }
};

/**
 * Assignment of an expression to a ghost variable.
 * Also used for given-bindings.
 */
struct GhostAssign {
    // Name of the ghost variable
    std::string varName;
    SrcLoc loc;
    llvm::Function *wrapperFunction;
    llvm::SmallVector<llvm::DILocalVariable *> givenArgs;
    llvm::SmallVector<llvm::DILocalVariable *> yieldsArgs;
    llvm::SmallVector<llvm::DILocalVariable *> wrapperArgs;

    GhostAssign(const std::string &varName, const SrcLoc &loc,
                llvm::Function *wrapperFunction)
        : varName(varName), loc(loc), wrapperFunction(wrapperFunction) {}

    void addWrapperArg(llvm::DILocalVariable *v) { wrapperArgs.push_back(v); }

    void addGivenArg(llvm::DILocalVariable *v) { givenArgs.push_back(v); }

    void addYieldsArg(llvm::DILocalVariable *v) { yieldsArgs.push_back(v); }
};

/**
 * Block of assignments to ghost variables.
 * Also used for given bindings.
 */
struct GhostAssignBlock {
    SrcLoc loc;
    llvm::SmallVector<GhostAssign> assignments;

    GhostAssignBlock(const SrcLoc &loc) : loc(loc) {}

    void addAssignment(GhostAssign assign) { assignments.push_back(assign); }
};

} // namespace pallas::irspec

#endif // PALLAS_IRSPEC_H
