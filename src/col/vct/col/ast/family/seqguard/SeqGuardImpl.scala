package vct.col.ast.family.seqguard

import vct.col.ast.{Endpoint, Expr, SeqGuard}

trait SeqGuardImpl[G] { this: SeqGuard[G] =>
  def condition: Expr[G]
  def endpointOpt: Option[Endpoint[G]]
}
