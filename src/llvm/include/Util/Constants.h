#ifndef PALLAS_CONSTANTS_H
#define PALLAS_CONSTANTS_H
#include <string>
/**
 * Useful string constants to use for searching out metadata nodes
 */
namespace pallas::constants {
const std::string VC_PREFIX = "VC.";

const std::string METADATA_PURE_KEYWORD = VC_PREFIX + "pure";
const std::string METADATA_CONTRACT_KEYWORD = VC_PREFIX + "contract";
const std::string METADATA_GLOBAL_KEYWORD = VC_PREFIX + "global";
} // namespace pallas::constants

#endif // PALLAS_CONSTANTS_H
