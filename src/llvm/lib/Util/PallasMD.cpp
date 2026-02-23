#include "Util/PallasMD.h"
#include "Util/Constants.h"
#include <llvm/Support/Casting.h>

#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>

namespace pallas::utils {

bool hasVcllvmContract(const llvm::Function &f) {
    return f.hasMetadata(pallas::constants::METADATA_CONTRACT_KEYWORD);
}

} // namespace pallas::utils
