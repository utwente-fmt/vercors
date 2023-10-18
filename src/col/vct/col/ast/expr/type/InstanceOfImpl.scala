package vct.col.ast.expr.`type`

import vct.col.ast.{InstanceOf, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait InstanceOfImpl[G] {
  this: InstanceOf[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc =
    assoc(value) <+> "instanceof" <+> typeValue
}
