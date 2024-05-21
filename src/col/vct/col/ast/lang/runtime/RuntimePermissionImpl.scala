package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Empty, _}
import vct.col.ast.ops.RuntimePermissionOps

trait RuntimePermissionImpl[G] extends RuntimePermissionOps[G] {
  this: RuntimePermission[G] =>

  override def t: Type[G] = TFraction[G]()

  def layoutPermission(implicit ctx: Ctx): Doc = {
    permission match {
      case _: WritePerm[G] => Text("1")
      case _: ReadPerm[G] => Text("0")
      case d : RatDiv[G] => Text("Fraction.getFraction(") <> d.left.show <> "," <+> d.right.show <> ")"
      case _ => Empty
    }
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(layoutPermission)
}