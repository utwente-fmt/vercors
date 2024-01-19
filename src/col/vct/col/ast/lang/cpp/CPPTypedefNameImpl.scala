package vct.col.ast.lang.cpp

import vct.col.ast.CPPTypedefName
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CPPTypedefNameOps

trait CPPTypedefNameImpl[G] extends CPPTypedefNameOps[G] { this: CPPTypedefName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Group(Text(nestedName) <>
    (if (genericArgs.nonEmpty) (Text("<") <> Doc.args(genericArgs) <> Text(">")) else Text("")))

  def appendName(postfix: String): CPPTypedefName[G] = {
    nestedName ++= postfix
    this
  }
}