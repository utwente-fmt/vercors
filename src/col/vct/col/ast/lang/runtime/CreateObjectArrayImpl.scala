package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CreateObjectArrayOps

trait CreateObjectArrayImpl[G] extends CreateObjectArrayOps[G] {
  this: CreateObjectArray[G] =>

  override val t: Type[G] = TArray[G](TAnyClass[G]())
  override def precedence: Int = Precedence.ATOMIC


  override def layout(implicit ctx: Ctx): Doc = Text("new Object[]{") <> Doc.args(args) <> "}"
}