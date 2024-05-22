package vct.col.ast.family.parregion

import vct.col.ast.{Node, ParRegion}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{Blame, ParPreconditionFailed}
import vct.col.ast.ops.ParRegionFamilyOps

trait ParRegionImpl[G] extends NodeFamilyImpl[G] with ParRegionFamilyOps[G] { this: ParRegion[G] =>
  def blame: Blame[ParPreconditionFailed]

  override def enterCheckContextRoScopes(context: CheckContext[G]): Int =
    context.scopes.size

  override def enterCheckContextRoScopeReason(context: CheckContext[G]): Option[Node[G]] =
    Some(this)
}
