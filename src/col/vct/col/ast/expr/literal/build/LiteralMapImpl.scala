package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralMap, TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.LiteralMapOps

trait LiteralMapImpl[G] extends LiteralMapOps[G] {
  this: LiteralMap[G] =>
  override def t: Type[G] = TMap(k, v)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("map<") <> k <> "," <+> v <> ">{" <> Doc.args(values.map {
      case (k, v) => k.show <+> "->" <+> v
    }) <> "}")
}
