package vct.col.ast.expr.resource

import vct.col.ast.{TResource, Type, Value}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ValueImpl[G] { this: Value[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("Value(") <> Doc.arg(loc) <> ")")
}
