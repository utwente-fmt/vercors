package vct.col.ast.lang.pvl

import vct.col.ast.PVLNamedType
import vct.col.print.{Ctx, Doc, Text, Empty, Group}
import vct.col.ast.ops.PVLNamedTypeOps

trait PVLNamedTypeImpl[G] extends PVLNamedTypeOps[G] {
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
