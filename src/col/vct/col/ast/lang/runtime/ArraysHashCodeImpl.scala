package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.ArraysHashCodeOps

trait ArraysHashCodeImpl[G] extends ArraysHashCodeOps[G] {
  this: ArraysHashCode[G] =>
  override def precedence: Int = Precedence.ATOMIC

  override val t: Type[G] = TInt[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("Arrays.hashCode(") <> obj <> ")"
}