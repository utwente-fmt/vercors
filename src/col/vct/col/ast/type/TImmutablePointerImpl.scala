package vct.col.ast.`type`

import vct.col.ast.{TImmutablePointer, Type}
import vct.col.ast.ops.TImmutablePointerOps
import vct.col.print._

trait TImmutablePointerImpl[G] extends TImmutablePointerOps[G] {
  this: TImmutablePointer[G] =>
  val unique: Option[BigInt] = None

  val isImmutable = true
  val isNonNull = false

  override def layout(implicit ctx: Ctx): Doc =
    Text("immutable_pointer") <> open <> element <> close

  override def asNullable: Type[G] = this
}
