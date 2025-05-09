package vct.col.ast.`type`

import vct.col.ast.{TConst, TNonNullConstPointer, Type}
import vct.col.ast.ops.TNonNullConstPointerOps
import vct.col.print._

trait TNonNullConstPointerImpl[G] extends TNonNullConstPointerOps[G] {
  this: TNonNullConstPointer[G] =>
  val unique: Option[BigInt] = None

  val isConst = true
  val isNonNull = true

  override def layout(implicit ctx: Ctx): Doc =
    Text("constNonNullPointer") <> open <> element <> close
}
