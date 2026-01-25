package vct.col.ast.`type`

import vct.col.ast.{TImmutablePointerArray, TNonNullImmutablePointerArray}
import vct.col.ast.ops.TNonNullImmutablePointerArrayOps
import vct.col.print._

trait TNonNullImmutablePointerArrayImpl[G]
    extends TNonNullImmutablePointerArrayOps[G] {
  this: TNonNullImmutablePointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("NonNull") <+> "immutable" <+> dimensions.foldLeft[Doc](element.show) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val unique: Option[BigInt] = None
  override val isImmutable: Boolean = true
  override val isNonNull: Boolean = true

  override def descend: TNonNullImmutablePointerArray[G] =
    TNonNullImmutablePointerArray(element, dimensions.tail)
  override def asNonNull: TNonNullImmutablePointerArray[G] = this
  override def asNullable: TImmutablePointerArray[G] =
    TImmutablePointerArray(element, dimensions)
}
