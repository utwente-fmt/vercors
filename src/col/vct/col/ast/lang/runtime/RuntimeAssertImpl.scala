package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeAssertImpl[G] {
  this: RuntimeAssert[G] =>


  override def layout(implicit ctx: Ctx): Doc = Text("assert (") <> res.show <> ")" <+> ": \"" <> message <> "\";"
}