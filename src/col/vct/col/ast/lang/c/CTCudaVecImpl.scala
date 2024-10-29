package vct.col.ast.lang.c

import vct.col.ast.CTCudaVec
import vct.col.ast.ops.CTCudaVecOps

trait CTCudaVecImpl[G] extends CTCudaVecOps[G] {
  this: CTCudaVec[G] =>

}
