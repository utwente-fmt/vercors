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
 * Types of contract clauses in the specification fromat of Pallas.
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

// TODO: Add spec statements and invariants

} // namespace pallas::irspec

#endif // PALLAS_IRSPEC_H
