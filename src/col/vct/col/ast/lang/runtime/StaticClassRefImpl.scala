package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait StaticClassRefImpl[G] {
  this: StaticClassRef[G] =>


  override def t: Type[G] = TClass(cls)
  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = t.show
}