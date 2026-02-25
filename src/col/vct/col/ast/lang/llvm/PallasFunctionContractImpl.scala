package vct.col.ast.lang.llvm

import vct.col.ast.PallasFunctionContract
import vct.col.ast.ops.PallasFunctionContractOps

trait PallasFunctionContractImpl[G] extends PallasFunctionContractOps[G] {
  this: PallasFunctionContract[G] =>
  override def givenArgs = content.givenArgs
  override def yieldsArgs = content.yieldsArgs
}
