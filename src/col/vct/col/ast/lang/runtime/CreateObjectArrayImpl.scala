package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait CreateObjectArrayImpl[G] {
  this: CreateObjectArray[G] =>

  override val t: Type[G] = TArray[G](TAnyClass[G]())
  override def precedence: Int = Precedence.ATOMIC


  override def layout(implicit ctx: Ctx): Doc = Text("{") <> Doc.args(args) <> "}"
}