package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeAssertExpectedOps

trait RuntimeAssertExpectedImpl[G] extends RuntimeAssertExpectedOps[G] {
  this: RuntimeAssertExpected[G] =>


  override def layout(implicit ctx: Ctx): Doc =
    Text("assert(") <> res.show <> ") : \"" <> message <+> " expected: \" +" <+> expected.show <+> "+ \" but got: \" +" <+>  received.show <> ";"
}