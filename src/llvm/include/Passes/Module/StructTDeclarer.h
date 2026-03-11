#ifndef PALLAS_STRUCTTDECLARER_H
#define PALLAS_STRUCTTDECLARER_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/PassManager.h>
#include <map>
#include <optional>
#include <utility>

/**
 * Pass that creates declares the LLVMStructDeclarations
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class SDResult {
    friend class StructTDeclarer;

  public:
    typedef std::pair<llvm::Type *, llvm::DIType *> StructTyID;

    explicit SDResult(llvm::Module &m, vct::col::ast::Program &colProg);

    /**
     * Return the ID for the given struct declaration.
     * If the declaration has not yet been transformed, it will be transformed
     * to COL.
     */
    std::optional<int64_t> getStructDeclId(StructTyID typeID);

    bool invalidate(Module &M, const PreservedAnalyses &PA,
                    ModuleAnalysisManager::Invalidator &);

  private:
    llvm::Module &m;

    // COL program into which the struct declarations are placed
    vct::col::ast::Program &colProg;

    // Set of StructTypeIDs for which a declaration is currently being
    // constructed or has already bee constructed
    // (required to avoid infinite recursion)
    std::set<StructTyID> transformedDecls;

    // Map StructTyID to the internally used IDs
    std::map<StructTyID, int64_t> internalIDMap;

    // Counter to issue unique ids
    int64_t nextID = 1;

    bool transformDecl(StructTyID typeID);

    /**
     * Initialize struct declaration based on the given llvm-type alone
     * (i.e. without considering debug-info).
     */
    void transformSDecl(col::LlvmStructDeclaration &decl, llvm::Type &llvmType);

    /**
     * Initialize struct declaratio based on the given llvm- and dbg-type.
     */
    bool transformSDeclWithDiType(col::LlvmStructDeclaration &decl,
                                  llvm::Type *llvmType, llvm::DIType &diType);

    int64_t getId(StructTyID &typeID);
};

class StructTDeclarer : public AnalysisInfoMixin<StructTDeclarer> {
    friend AnalysisInfoMixin<StructTDeclarer>;
    static AnalysisKey Key;

  public:
    using Result = SDResult;

    Result run(Module &M, ModuleAnalysisManager &MAM);
};

} // namespace pallas
#endif // PALLAS_STRUCTTDECLARER_H
