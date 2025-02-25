#ifndef PALLAS_BLOCKUTILS_H
#define PALLAS_BLOCKUTILS_H

#include "vct/col/ast/col.pb.h"

namespace pallas {
namespace col = vct::col::ast;

col::Block &bodyAsBlock(col::LlvmBasicBlock &llvmBB);

} // namespace pallas
#endif // PALLAS_BLOCKUTILS_H
