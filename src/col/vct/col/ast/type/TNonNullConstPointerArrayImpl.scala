package vct.col.ast.`type`

import vct.col.ast.{TConstPointerArray, TNonNullConstPointerArray}
import vct.col.ast.ops.TNonNullConstPointerArrayOps
import vct.col.print._

trait TNonNullConstPointerArrayImpl[G] extends TNonNullConstPointerArrayOps[G] {
  this: TNonNullConstPointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("NonNull") <+> "const" <+> dimensions.foldLeft[Doc](element.show) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val unique: Option[BigInt] = None
  override val isConst: Boolean = true
  override val isNonNull: Boolean = true

  override def descend: TNonNullConstPointerArray[G] =
    TNonNullConstPointerArray(element, dimensions.tail)
  override def asNonNull: TNonNullConstPointerArray[G] = this
  override def asNullable: TConstPointerArray[G] =
    TConstPointerArray(element, dimensions)
}
