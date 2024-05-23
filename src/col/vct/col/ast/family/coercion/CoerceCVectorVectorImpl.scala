package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceCVectorVectorOps
import vct.col.ast.{CoerceCVectorVector, TVector}

trait CoerceCVectorVectorImpl[G] extends CoerceCVectorVectorOps[G] {
  this: CoerceCVectorVector[G] =>
  override def target: TVector[G] = TVector(size, elementType)()
  // override def layout(implicit ctx: Ctx): Doc = ???
}
