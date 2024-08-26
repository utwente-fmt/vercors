package vct.col.ast.declaration.global

import vct.col.ast.{ByValueClass, Expr, InstanceField, TByValueClass, Type}
import vct.col.ast.ops.ByValueClassOps
import vct.col.util.AstBuildHelpers._

trait ByValueClassImpl[G] extends ByValueClassOps[G] {
  this: ByValueClass[G] =>
  override def intrinsicLockInvariant: Expr[G] = tt
  override def classType(typeArgs: Seq[Type[G]]): TByValueClass[G] =
    TByValueClass[G](this.ref, typeArgs)
}
