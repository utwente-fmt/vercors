package vct.col.ast.family.seqguard

import vct.col.ast.UnpointedGuard
import vct.col.ast.ops.UnpointedGuardOps

trait UnpointedGuardImpl[G] extends UnpointedGuardOps[G] { this: UnpointedGuard[G] =>
  def endpointOpt = None
}
