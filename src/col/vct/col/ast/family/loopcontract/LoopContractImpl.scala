package vct.col.ast.family.loopcontract

import vct.col.ast.LoopContract
import vct.col.ast.ops.LoopContractFamilyOps

trait LoopContractImpl[G] extends LoopContractFamilyOps[G] {
  this: LoopContract[G] =>

}
