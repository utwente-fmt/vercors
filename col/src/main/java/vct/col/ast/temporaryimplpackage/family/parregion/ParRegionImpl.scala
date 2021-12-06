package vct.col.ast.temporaryimplpackage.family.parregion

import vct.col.ast.ParRegion
import vct.col.origin.{Blame, ParPreconditionFailed}

trait ParRegionImpl { this: ParRegion =>
  def blame: Blame[ParPreconditionFailed]
}
