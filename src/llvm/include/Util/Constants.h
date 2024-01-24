#ifndef VCLLVM_CONSTANTS_H
#define VCLLVM_CONSTANTS_H
#include <string>
/**
 * Useful string constants to use for searching out metadata nodes
 */
namespace vcllvm::constants {
    const std::string VC_PREFIX = "VC.";

    const std::string METADATA_PURE_KEYWORD = VC_PREFIX + "pure";
    const std::string METADATA_CONTRACT_KEYWORD = VC_PREFIX + "contract";
    const std::string METADATA_GLOBAL_KEYWORD = VC_PREFIX + "global";
}

#endif //VCLLVM_CONSTANTS_H
