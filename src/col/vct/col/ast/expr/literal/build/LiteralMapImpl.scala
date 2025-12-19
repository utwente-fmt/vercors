package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralMap, TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.LiteralMapOps

trait LiteralMapImpl[G] extends LiteralMapOps[G] {
  this: LiteralMap[G] =>
  override def t: Type[G] = TMap(k, v)

  override def precedence: Int = Precedence.POSTFIX

  def layoutIsar(implicit ctx: Ctx): Doc = {
    if (values.isEmpty) { Group(Text("Map.empty")) }
    else {
      Group(Text("[") <+> Doc.args(values.map { case (k, v) =>
        k.show <+> "↦" <+> v
      }) <+> Text("]"))
    }
  }

  def layoutPvl(implicit ctx: Ctx): Doc =
    Group(Text("map<") <> k <> "," <+> v <> ">{" <> Doc.args(values.map {
      case (k, v) => k.show <+> "->" <+> v
    }) <> "}")

  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => layoutIsar
      case _ => layoutPvl
    }

  }
}
