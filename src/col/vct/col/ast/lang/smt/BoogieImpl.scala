package vct.col.ast.lang.smt

import vct.col.ast.Boogie
import vct.col.ast.ops.BoogieOps
import vct.col.print._

trait BoogieImpl[G] extends BoogieOps[G] { this: Boogie[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
