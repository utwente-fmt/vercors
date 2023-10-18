package vct.col.ast.lang

import vct.col.ast.PVLNamedType
import vct.col.print.{Ctx, Doc, Text, Empty, Group}

trait PVLNamedTypeImpl[G] {
  this: PVLNamedType[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(name) <>
        (if (typeArgs.isEmpty)
           Empty
         else
           Text("<") <> Doc.args(typeArgs) <> ">")
    )
}
