#include "Util/BlockUtils.h"

namespace pallas {

col::Block &bodyAsBlock(col::LlvmBasicBlock &llvmBB) {
    return *llvmBB.mutable_body()->mutable_block();
}

} // namespace pallas
