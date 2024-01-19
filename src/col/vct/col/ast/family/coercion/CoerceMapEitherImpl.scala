package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapEither, TEither}
import vct.col.ast.ops.CoerceMapEitherOps

trait CoerceMapEitherImpl[G] extends CoerceMapEitherOps[G] { this: CoerceMapEither[G] => 
  override def target: TEither[G] = TEither(targetTypes._1, targetTypes._2)
}
