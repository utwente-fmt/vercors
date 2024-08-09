package vct.col.ast.family.coercion

import vct.col.ast.CoerceFromUnique
import vct.col.ast.ops.CoerceFromUniqueOps
import vct.col.print._

trait CoerceFromUniqueImpl[G] extends CoerceFromUniqueOps[G] { this: CoerceFromUnique[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
