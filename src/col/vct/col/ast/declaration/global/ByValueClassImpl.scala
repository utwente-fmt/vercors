package vct.col.ast.declaration.global

import vct.col.ast.{ByValueClass, TByValueClass, Type}
import vct.col.ast.ops.ByValueClassOps
import vct.col.origin.Origin
import vct.col.print._

trait ByValueClassImpl[G] extends ByValueClassOps[G] {
  this: ByValueClass[G] =>
  override def supports: Seq[Type[G]] = Nil
  override def classType(typeArgs: Seq[Type[G]])(
      implicit o: Origin = this.o
  ): TByValueClass[G] = TByValueClass[G](this.ref, typeArgs)(o)
  override def layoutLockInvariant(implicit ctx: Ctx): Doc = Empty
}
