package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCIntCFloat}
import vct.col.ast.ops.CoerceCIntCFloatOps

trait CoerceCIntCFloatImpl[G] extends CoerceCIntCFloatOps[G] {
  this: CoerceCIntCFloat[G] =>

}
