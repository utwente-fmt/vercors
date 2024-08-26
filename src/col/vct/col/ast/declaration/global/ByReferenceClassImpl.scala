package vct.col.ast.declaration.global

import vct.col.ast.{ByReferenceClass, TByReferenceClass, Type}
import vct.col.ast.ops.ByReferenceClassOps

trait ByReferenceClassImpl[G] extends ByReferenceClassOps[G] {
  this: ByReferenceClass[G] =>
  override def classType(typeArgs: Seq[Type[G]]): TByReferenceClass[G] =
    TByReferenceClass[G](this.ref, typeArgs)
}
