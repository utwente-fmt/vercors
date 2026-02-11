package vct.col.ast.`type`

import vct.col.ast.{TImmutablePointerArray, TNonNullImmutablePointerArray}
import vct.col.ast.ops.TImmutablePointerArrayOps
import vct.col.print._

trait TImmutablePointerArrayImpl[G] extends TImmutablePointerArrayOps[G] {
  this: TImmutablePointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("immutable") <+> dimensions.foldLeft[Doc](element.show) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val unique: Option[BigInt] = None
  override val isImmutable: Boolean = true
  override val isNonNull: Boolean = false

  override def descend: TImmutablePointerArray[G] =
    TImmutablePointerArray(element, dimensions.tail)
  override def asNonNull: TNonNullImmutablePointerArray[G] =
    TNonNullImmutablePointerArray(element, dimensions)
  override def asNullable: TImmutablePointerArray[G] = this
}
