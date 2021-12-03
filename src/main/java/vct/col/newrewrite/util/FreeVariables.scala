package vct.col.newrewrite.util

import vct.col.ast.{ThisObject, ThisModel, Local, Node}
import vct.col.check.CheckContext

object FreeVariables {
  sealed trait FreeVariable
  case class FreeVar(v: Local) extends FreeVariable
  case class FreeThisObject(t: ThisObject) extends FreeVariable
  case class FreeThisModel(t: ThisModel) extends FreeVariable

  /**
   * Scans the values (variables) that are defined outside this node
   */
  def freeVariables(node: Node, scope: CheckContext = CheckContext()): Set[FreeVariable] = node match {
    case local @ Local(ref) => if(scope.inScope(ref)) Set.empty else Set(FreeVar(local))
    case diz @ ThisObject(_) => Set(FreeThisObject(diz))
    case diz @ ThisModel(_) => Set(FreeThisModel(diz))
    case other => other.subnodes.flatMap(freeVariables(_, other.enterCheckContext(scope))).toSet
  }
}
