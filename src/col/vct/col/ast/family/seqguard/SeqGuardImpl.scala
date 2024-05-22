package vct.col.ast.family.seqguard

import vct.col.ast.{Endpoint, Expr, SeqGuard, UnpointedGuard}
import vct.col.ast.ops.SeqGuardFamilyOps

trait SeqGuardImpl[G] extends SeqGuardFamilyOps[G] { this: SeqGuard[G] =>
  def condition: Expr[G]
  def endpointOpt: Option[Endpoint[G]]
}
