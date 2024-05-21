package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.TRuntimeFractionOps

trait TRuntimeFractionImpl[G] extends TRuntimeFractionOps[G] {
  this: TRuntimeFraction[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Fraction")
}