package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.ObjectGetLengthOps

trait ObjectGetLengthImpl[G] extends ObjectGetLengthOps[G] {
  this: ObjectGetLength[G] =>

  override val t: Type[G] = TInt[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("Array.getLength(") <> input.show <> ")"
}