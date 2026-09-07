package vct.col.ast.unsorted

import vct.col.ast.PredicateDefinition
import vct.col.ast.ops.PredicateDefinitionOps
import vct.col.print._

trait PredicateDefinitionImpl[G] extends PredicateDefinitionOps[G] {
  this: PredicateDefinition[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
