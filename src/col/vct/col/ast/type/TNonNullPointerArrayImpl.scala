package vct.col.ast.`type`

import vct.col.ast.{TNonNullPointerArray, TPointerArray}
import vct.col.ast.ops.TNonNullPointerArrayOps
import vct.col.print._

trait TNonNullPointerArrayImpl[G] extends TNonNullPointerArrayOps[G] {
  this: TNonNullPointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    dimensions.foldLeft[Doc](
      Text("NonNull") <+> unique.map(u => Text(s"unique<$u>") <+> element)
        .getOrElse(element.show)
    ) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val isImmutable: Boolean = false
  override val isNonNull: Boolean = true

  override def descend: TNonNullPointerArray[G] =
    TNonNullPointerArray(element, dimensions.tail, unique)
  override def asNonNull: TNonNullPointerArray[G] = this
  override def asNullable: TPointerArray[G] =
    TPointerArray(element, dimensions, unique)
}
