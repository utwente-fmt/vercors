package vct.col.ast.`type`

import vct.col.ast.{TNonNullPointerArray, TPointerArray}
import vct.col.ast.ops.TPointerArrayOps
import vct.col.print._

trait TPointerArrayImpl[G] extends TPointerArrayOps[G] {
  this: TPointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    dimensions.foldLeft[Doc](
      unique.map(u => Text(s"unique<$u>") <+> element).getOrElse(element.show)
    ) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val isConst: Boolean = false
  override val isNonNull: Boolean = false

  override def descend: TPointerArray[G] =
    TPointerArray(element, dimensions.tail, unique)
  override def asNonNull: TNonNullPointerArray[G] =
    TNonNullPointerArray(element, dimensions, unique)
  override def asNullable: TPointerArray[G] = this
}
