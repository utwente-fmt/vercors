package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapEither, TEither}

trait CoerceMapEitherImpl[G] { this: CoerceMapEither[G] => 
  override def target: TEither[G] = TEither(targetTypes._1, targetTypes._2)
}
