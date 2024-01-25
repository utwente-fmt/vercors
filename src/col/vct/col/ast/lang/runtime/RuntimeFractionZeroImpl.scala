package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeFractionZeroImpl[G] {
  this: RuntimeFractionZero[G] =>

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Text("Fraction.ZERO")
}