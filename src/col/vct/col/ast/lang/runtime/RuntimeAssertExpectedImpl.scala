package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeAssertExpectedImpl[G] {
  this: RuntimeAssertExpected[G] =>


  override def layout(implicit ctx: Ctx): Doc =
    Text("assert(") <> res.show <> ") : \"" <> message <+> " expected: \" +" <+> expected.show <+> "+ \" but got: \" +" <+>  received.show <> ";"
}