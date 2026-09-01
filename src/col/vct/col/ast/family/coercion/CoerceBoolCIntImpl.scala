package vct.col.ast.family.coercion

import vct.col.ast.CoerceBoolCInt
import vct.col.ast.ops.CoerceBoolCIntOps

trait CoerceBoolCIntImpl[G] extends CoerceBoolCIntOps[G] {
  this: CoerceBoolCInt[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
