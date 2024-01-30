package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait ObjectGetLengthImpl[G] {
  this: ObjectGetLength[G] =>

  override val t: Type[G] = TInt[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("Array.getLength(") <> input.show <> ")"
}