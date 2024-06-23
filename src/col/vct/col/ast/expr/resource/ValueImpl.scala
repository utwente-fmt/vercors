package vct.col.ast.expr.resource

import vct.col.ast.{Deref, FieldLocation, TResource, Type, Value}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.ValueOps

trait ValueImpl[G] extends ValueOps[G] {
  this: Value[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("Value(") <> Doc.arg(loc) <> ")")
}
