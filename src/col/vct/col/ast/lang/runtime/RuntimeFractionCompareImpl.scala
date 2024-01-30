package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeFractionCompareImpl[G] {
  this: RuntimeFractionCompare[G] =>

  override def t: Type[G] = TInt[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    left.show <> Text(".compareTo(") <> right <> Text(")")
}