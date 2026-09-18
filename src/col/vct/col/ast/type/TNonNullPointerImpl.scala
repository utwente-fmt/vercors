package vct.col.ast.`type`

import vct.col.ast.{TNonNullPointer, TPointer, Type}
import vct.col.ast.ops.TNonNullPointerOps
import vct.col.print._

trait TNonNullPointerImpl[G] extends TNonNullPointerOps[G] {
  this: TNonNullPointer[G] =>

  val isConst = false
  val isNonNull = true

  override def layoutSplitDeclarator(implicit ctx: Ctx): (Doc, Doc) = {
    val (spec, decl) = element.layoutSplitDeclarator
    (spec, decl <> "*")
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(
        (if (unique.isDefined)
           "unique<" + unique.get.toString + ">"
         else
           "") + "NonNull"
      ) <> open <> element <> close
    )

  override def asNullable: Type[G] = TPointer(element, unique)
}
