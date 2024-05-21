package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeAssertOps

trait RuntimeAssertImpl[G] extends RuntimeAssertOps[G] {
  this: RuntimeAssert[G] =>


  override def layout(implicit ctx: Ctx): Doc = Group(Group(Text("assert (") <> res.show <> ")" ) <> Group(Text(": \"") <> message <> "\";"))
}