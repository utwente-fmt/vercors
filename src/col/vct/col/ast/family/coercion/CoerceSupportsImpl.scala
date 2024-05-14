package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSupports, TClass}
import vct.col.ast.ops.CoerceSupportsOps

trait CoerceSupportsImpl[G] extends CoerceSupportsOps[G] {
  this: CoerceSupports[G] =>
  // TODO (RR): Integrate coercions with generics?
  override def target: TClass[G] =
    targetClass.decl.classType({
      assert(sourceClass.decl.typeArgs.isEmpty); Seq()
    })
}
