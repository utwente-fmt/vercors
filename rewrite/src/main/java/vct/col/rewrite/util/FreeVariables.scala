package vct.col.rewrite.util

import vct.col.ast.{Assign, Local, Node, PostAssignExpression, PreAssignExpression, TVar, ThisModel, ThisObject}
import vct.col.check.CheckContext

object FreeVariables {
  sealed trait FreeVariable[G]
  case class ReadFreeVar[G](v: Local[G]) extends FreeVariable[G]
  case class ReadTypeVar[G](v: TVar[G]) extends FreeVariable[G]
  case class WriteFreeVar[G](v: Local[G]) extends FreeVariable[G]
  case class FreeThisObject[G](t: ThisObject[G]) extends FreeVariable[G]
  case class FreeThisModel[G](t: ThisModel[G]) extends FreeVariable[G]

  /**
   * Scans the values (variables) that are defined outside this node
   */
  def freeVariables[G](node: Node[G], scope: CheckContext[G] = CheckContext[G]()): Set[FreeVariable[G]] = node match {
    case local @ Local(ref) if(!scope.inScope(ref)) => Set(ReadFreeVar(local))
    case tVar @ TVar(ref) if !scope.inScope(ref) => Set(ReadTypeVar(tVar))
    case Assign(local @ Local(ref), _) if(!scope.inScope(ref)) => Set(WriteFreeVar(local))
    case PreAssignExpression(local @ Local(ref), _) if(!scope.inScope(ref)) => Set(WriteFreeVar(local))
    case PostAssignExpression(local @ Local(ref), _) if(!scope.inScope(ref)) => Set(WriteFreeVar(local))
    case diz: ThisObject[G] => Set(FreeThisObject(diz))
    case diz: ThisModel[G] => Set(FreeThisModel(diz))
    case other => other.subnodes.flatMap(freeVariables(_, other.enterCheckContext(scope))).toSet
  }
}
