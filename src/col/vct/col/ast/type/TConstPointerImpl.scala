package vct.col.ast.`type`

import vct.col.ast.{TConstPointer, TConst, Type}
import vct.col.ast.ops.TConstPointerOps
import vct.col.print._

trait TConstPointerImpl[G] extends TConstPointerOps[G] {
  this: TConstPointer[G] =>
  val unique: Option[BigInt] = None

  val isConst = true
  val isNonNull = false

  override def layout(implicit ctx: Ctx): Doc =
    Text("const_pointer") <> open <> element <> close

  override def asNullable: Type[G] = this
}
