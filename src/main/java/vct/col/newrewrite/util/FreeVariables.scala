package vct.col.newrewrite.util

import vct.col.ast.{ThisObject, ThisModel, Local, Node}
import vct.col.check.CheckContext

object FreeVariables {
  sealed trait FreeVariable[G]
  case class FreeVar[G](v: Local[G]) extends FreeVariable[G]
  case class FreeThisObject[G](t: ThisObject[G]) extends FreeVariable[G]
  case class FreeThisModel[G](t: ThisModel[G]) extends FreeVariable[G]

  /**
   * Scans the values (variables) that are defined outside this node
   */
  def freeVariables[G](node: Node[G], scope: CheckContext[G] = CheckContext[G]()): Set[FreeVariable[G]] = node match {
    case local @ Local(ref) => if(scope.inScope(ref)) Set.empty else Set(FreeVar(local))
    case diz @ ThisObject(_) => Set(FreeThisObject(diz))
    case diz @ ThisModel(_) => Set(FreeThisModel(diz))
    case other => other.subnodes.flatMap(freeVariables(_, other.enterCheckContext(scope))).toSet
  }
}
