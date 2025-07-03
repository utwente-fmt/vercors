package vct.col.ast.`type`

import vct.col.ast.{TConstPointerArray, TNonNullConstPointerArray}
import vct.col.ast.ops.TConstPointerArrayOps
import vct.col.print._

trait TConstPointerArrayImpl[G] extends TConstPointerArrayOps[G] {
  this: TConstPointerArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("const") <+> dimensions.foldLeft[Doc](element.show) {
      case (l, Some(r)) => l <> "[" <> r <> "]"
      case (l, None) => l <> "[]"
    }

  override val unique: Option[BigInt] = None
  override val isConst: Boolean = true
  override val isNonNull: Boolean = false

  override def descend: TConstPointerArray[G] =
    TConstPointerArray(element, dimensions.tail)
  override def asNonNull: TNonNullConstPointerArray[G] =
    TNonNullConstPointerArray(element, dimensions)
  override def asNullable: TConstPointerArray[G] = this
}
