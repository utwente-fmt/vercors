package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeFractionOneImpl[G] {
  this: RuntimeFractionOne[G] =>

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Text("Fraction.ONE")
}