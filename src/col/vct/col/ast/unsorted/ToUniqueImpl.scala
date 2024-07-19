package vct.col.ast.unsorted

import vct.col.ast.{TAny, ToUnique}
import vct.col.ast.ops.ToUniqueOps
import vct.col.print._

trait ToUniqueImpl[G] extends ToUniqueOps[G] { this: ToUnique[G] =>
  // Solve typing immediately in TypeQualifiersCoercion pass
  def t: vct.col.ast.Type[G] = TAny[G]()
}
