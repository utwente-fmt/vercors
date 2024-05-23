package vct.col.ast.expr.`type`

import vct.col.ast.{TType, Type, TypeValue}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.TypeValueOps

trait TypeValueImpl[G] extends TypeValueOps[G] { this: TypeValue[G] =>
  override def t: Type[G] = TType(value)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\type(") <> Doc.arg(value) <> ")")
}