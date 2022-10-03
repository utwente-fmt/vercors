package vct.col.ast.family.parregion

import vct.col.ast.ParRegion
import vct.col.origin.{Blame, ParPreconditionFailed}

trait ParRegionImpl[G] { this: ParRegion[G] =>
  def blame: Blame[ParPreconditionFailed]
}
