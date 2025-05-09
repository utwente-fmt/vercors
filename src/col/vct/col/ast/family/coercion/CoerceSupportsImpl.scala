package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSupports, TClass}
import vct.col.ast.ops.CoerceSupportsOps

trait CoerceSupportsImpl[G] extends CoerceSupportsOps[G] {
  this: CoerceSupports[G] =>
  // TODO: Generics are not properly taken into account here
  override def target: TClass[G] =
    targetClass.decl.classType({
      assert(sourceClass.decl.typeArgs.isEmpty); Seq()
    })
}
