package vct.col.ast.lang

import vct.col.ast.CPPTypedefName
import vct.col.print.{Ctx, Doc, Text}

trait CPPTypedefNameImpl[G] {
  this: CPPTypedefName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(nestedName)

  def appendName(postfix: String): CPPTypedefName[G] = {
    nestedName ++= postfix
    this
  }
}
