package vct.col.ast.lang

import vct.col.ast.CPPTypedefName
import vct.col.print.{Ctx, Doc, Group, Text}

trait CPPTypedefNameImpl[G] { this: CPPTypedefName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Group(Text(nestedName) <>
    (if (genericArgs.nonEmpty) (Text("<") <> Text(genericArgs.mkString(", ")) <> Text(">")) else Text("")))

  def appendName(postfix: String): CPPTypedefName[G] = {
    nestedName ++= postfix
    this
  }
}