package vct.col.ast.`type`.typeclass

import vct.col.ast.IntType
import vct.col.print.{Ctx, Doc, Text}

trait IntTypeImpl[G] {
  this: IntType[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}
