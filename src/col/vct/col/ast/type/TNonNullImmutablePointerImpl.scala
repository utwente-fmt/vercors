package vct.col.ast.`type`

import vct.col.ast.{TImmutablePointer, TNonNullImmutablePointer, Type}
import vct.col.ast.ops.TNonNullImmutablePointerOps
import vct.col.print._

trait TNonNullImmutablePointerImpl[G] extends TNonNullImmutablePointerOps[G] {
  this: TNonNullImmutablePointer[G] =>
  val unique: Option[BigInt] = None

  val isImmutable = true
  val isNonNull = true

  override def layout(implicit ctx: Ctx): Doc =
    Text("immutableNonNullPointer") <> open <> element <> close

  override def asNullable: Type[G] = TImmutablePointer(element)
}
