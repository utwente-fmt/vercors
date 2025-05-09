#ifndef PALLAS_PUREASSIGNER_H
#define PALLAS_PUREASSIGNER_H

#include "vct/col/ast/col.pb.h"
#include <llvm/IR/PassManager.h>
/**
 * The PureAssignerPass checks if a LLVM function is pure (i.e. whether the
 * !VC.pure metadata node is set)
 */
namespace pallas {
using namespace llvm;
namespace col = vct::col::ast;

class PureAssignerPass : public PassInfoMixin<PureAssignerPass> {
  public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM);

  private:
    /**
     * Checks if the given metadata-node represents a valid pure-annotation in
     * the format that is used by VCLLVM.
     * If the metadata is not wellformed, an error is added to the ErrorReporter
     * and false is returned.
     * The given function is used to build error-messages.
     */
    bool isVcllvmPureWellformed(MDNode &pureMD, Function &f);

    /**
     * Checks if the given metadata-node represents a valid pure-annotation
     * inside of a function-contract  in the format that is used by Pallas. If
     * the metadata is not wellformed, an error is added to the ErrorReporter
     * and false is returned.
     * The given function is used to build error-messages.
     */
    bool isPallasPureWellformed(MDNode &contractMD, Function &f);

    /**
     * Extracts the pure-value from a metadata-node that represents a pallas
     * function contract.
     * Assumes, that the metadata-node is wellformed. (I.e. it has at least two
     * operands, and the second operand is a boolean constant)
     */
    bool getPureValueFromPallasContract(MDNode &contractMD);
};
} // namespace pallas
#endif // PALLAS_PUREASSIGNER_H
