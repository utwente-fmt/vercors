package vct.col.ast.family.parregion

import vct.col.ast.{Blame1, ParRegion}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{Blame, ParPreconditionFailed}

trait ParRegionImpl[G] extends NodeFamilyImpl[G] { this: ParRegion[G] =>
  def blame: Blame1[G]

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context.copy(roScopes = context.scopes.size, roScopeReason = Some(this))
}
