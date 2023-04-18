package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralSet, TSet, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait LiteralSetImpl[G] { this: LiteralSet[G] =>
  override def t: Type[G] = TSet(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("set<") <> element <> ">{" <> Doc.args(values) <> "}")
}