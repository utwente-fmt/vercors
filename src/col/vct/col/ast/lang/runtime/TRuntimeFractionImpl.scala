package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait TRuntimeFractionImpl[G] {
  this: TRuntimeFraction[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Fraction")
}