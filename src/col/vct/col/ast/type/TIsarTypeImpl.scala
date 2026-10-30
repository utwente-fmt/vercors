package vct.col.ast.`type`

import vct.col.ast.{IsarDatatypeCommand, IsarTypedefCommand, TIsarType}
import vct.col.ast.ops.TIsarTypeOps
import vct.col.print.{Ctx, Doc, Empty, Group, Text}

trait TIsarTypeImpl[G] extends TIsarTypeOps[G] {
  this: TIsarType[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      if (args.isEmpty)
        Empty
      else
        Text("(") <> Doc.args(args) <> Text(")")
    ) <+> Text(this.adt.decl match {
      case d: IsarDatatypeCommand[G] => d.typename
      case t: IsarTypedefCommand[G] => t.typename
    })
}
