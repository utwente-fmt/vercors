package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralVector, TVector, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.LiteralVectorOps

trait LiteralVectorImpl[G] extends LiteralVectorOps[G] { this: LiteralVector[G] =>
  override def t: Type[G] = TVector(element, values.size)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("vector<") <> element <> "," <> values.size.toString <> ">{" <> Doc.args(values) <> "}")
}
