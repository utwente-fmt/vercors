package vct.col.ast.family.seqguard

import vct.col.ast.{Endpoint, Expr, ChorGuard, UnpointedGuard}
import vct.col.ast.ops.ChorGuardFamilyOps

trait ChorGuardImpl[G] extends ChorGuardFamilyOps[G] { this: ChorGuard[G] =>
  def condition: Expr[G]
  def endpointOpt: Option[Endpoint[G]]
}
