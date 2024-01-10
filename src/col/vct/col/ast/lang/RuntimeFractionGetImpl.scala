package vct.col.ast.lang

import vct.col.ast._
import vct.col.print._

trait RuntimeFractionGetImpl[G] {
  this: RuntimeFractionGet[G] =>

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Text("Fraction.getFraction(") <> left <> Text(",") <> right <> Text(")")
}