package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeFractionAddImpl[G] {
  this: RuntimeFractionAdd[G] =>

  override def t: Type[G] = TRuntimeFraction[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    left.show <> Text(".add(") <> right <> Text(")")
}