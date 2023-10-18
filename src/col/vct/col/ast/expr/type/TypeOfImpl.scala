package vct.col.ast.expr.`type`

import vct.col.ast.{TType, Type, TypeOf}
import vct.col.print.{Ctx, Doc, Precedence, Text, Group}

trait TypeOfImpl[G] {
  this: TypeOf[G] =>
  override def t: Type[G] = TType(expr.t)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\typeof(") <> Doc.arg(expr) <> ")")
}
