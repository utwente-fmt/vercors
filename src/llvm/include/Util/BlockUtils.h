#ifndef PALLAS_BLOCKUTILS_H
#define PALLAS_BLOCKUTILS_H

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#pragma GCC diagnostic ignored "-Woverflow"
#endif // __GNUC__
#include "vct/col/ast/col.pb.h"
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__

namespace pallas {
namespace col = vct::col::ast;

col::Block &bodyAsBlock(col::LlvmBasicBlock &llvmBB);

} // namespace pallas
#endif // PALLAS_BLOCKUTILS_H
