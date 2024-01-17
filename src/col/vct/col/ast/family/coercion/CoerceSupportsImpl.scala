package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSupports, TClass}
import vct.col.ast.ops.CoerceSupportsOps

trait CoerceSupportsImpl[G] extends CoerceSupportsOps[G] { this: CoerceSupports[G] => 
  override def target: TClass[G] = TClass(targetClass)
}
