package vct.col.ast.family.seqguard

import vct.col.ast.UnpointedGuard

trait UnpointedGuardImpl[G] { this: UnpointedGuard[G] =>
  def endpointOpt = None
}
