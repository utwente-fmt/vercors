package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.StaticClassRefOps

trait StaticClassRefImpl[G] extends StaticClassRefOps[G] {
  this: StaticClassRef[G] =>


  override def t: Type[G] = TClass(cls, Nil)
  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = t.show
}