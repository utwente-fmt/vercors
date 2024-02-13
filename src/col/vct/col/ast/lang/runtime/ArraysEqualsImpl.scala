package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait ArraysEqualsImpl[G] {
  this: ArraysEquals[G] =>
  override def precedence: Int = Precedence.ATOMIC

  override val t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("Arrays.equals(") <> obj <> "," <+> target.show <> ")"
}