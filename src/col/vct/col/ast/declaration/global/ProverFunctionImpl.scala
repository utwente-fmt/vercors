package vct.col.ast.declaration.global

import vct.col.ast.{Node, ProverFunction}
import vct.col.ast.ops.ProverFunctionOps

trait ProverFunctionImpl[G] extends ProverFunctionOps[G] {
  this: ProverFunction[G] =>
  override def body: None.type = None
}
