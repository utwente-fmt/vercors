package vct.col.rewrite.util

import vct.col.ast.{
  AnyMethodInvocation,
  Assign,
  HeapLocal,
  InvocationStatement,
  Local,
  Node,
  PostAssignExpression,
  PreAssignExpression,
  TVar,
  ThisModel,
  ThisObject,
}
import vct.col.check.CheckContext

object FreeVariables {
  sealed trait FreeVariable[G]
  case class ReadFreeVar[G](v: Local[G]) extends FreeVariable[G]
  case class ReadFreeHeapVar[G](v: HeapLocal[G]) extends FreeVariable[G]
  case class ReadTypeVar[G](v: TVar[G]) extends FreeVariable[G]
  case class WriteFreeVar[G](v: Local[G]) extends FreeVariable[G]
  case class WriteFreeHeapVar[G](v: HeapLocal[G]) extends FreeVariable[G]
  case class FreeThisObject[G](t: ThisObject[G]) extends FreeVariable[G]
  case class FreeThisModel[G](t: ThisModel[G]) extends FreeVariable[G]

  private def flatFreeVariables[G](
      node: Node[G],
      scope: CheckContext[G],
  ): Set[FreeVariable[G]] =
    node match {
      case local @ Local(ref) if !scope.inScope(ref) => Set(ReadFreeVar(local))
      case local @ HeapLocal(ref) if !scope.inScope(ref) =>
        Set(ReadFreeHeapVar(local))
      case tVar @ TVar(ref) if !scope.inScope(ref) => Set(ReadTypeVar(tVar))
      case Assign(local @ Local(ref), _) if !scope.inScope(ref) =>
        Set(WriteFreeVar(local))
      case PreAssignExpression(local @ Local(ref), _) if !scope.inScope(ref) =>
        Set(WriteFreeVar(local))
      case PostAssignExpression(local @ Local(ref), _) if !scope.inScope(ref) =>
        Set(WriteFreeVar(local))
      case Assign(local @ HeapLocal(ref), _) if !scope.inScope(ref) =>
        Set(WriteFreeHeapVar(local))
      case PreAssignExpression(local @ HeapLocal(ref), _)
          if !scope.inScope(ref) =>
        Set(WriteFreeHeapVar(local))
      case PostAssignExpression(local @ HeapLocal(ref), _)
          if !scope.inScope(ref) =>
        Set(WriteFreeHeapVar(local))
      case inv: InvocationStatement[G] =>
        inv.outArgs.collect {
          case l @ Local(ref) if !scope.inScope(ref) => WriteFreeVar(l)
          case l @ HeapLocal(ref) if !scope.inScope(ref) => WriteFreeHeapVar(l)
        }.toSet ++ inv.yields.collect {
          case (l @ Local(ref), _) if !scope.inScope(ref) => WriteFreeVar(l)
          case (l @ HeapLocal(ref), _) if !scope.inScope(ref) =>
            WriteFreeHeapVar(l)
        }.toSet
      case inv: AnyMethodInvocation[G] =>
        inv.outArgs.collect {
          case l @ Local(ref) if !scope.inScope(ref) => WriteFreeVar(l)
          case l @ HeapLocal(ref) if !scope.inScope(ref) => WriteFreeHeapVar(l)
        }.toSet ++ inv.yields.collect {
          case (l @ Local(ref), _) if !scope.inScope(ref) => WriteFreeVar(l)
          case (l @ HeapLocal(ref), _) if !scope.inScope(ref) =>
            WriteFreeHeapVar(l)
        }.toSet
      case diz: ThisObject[G] => Set(FreeThisObject(diz))
      case diz: ThisModel[G] => Set(FreeThisModel(diz))
      case _ => Set.empty
    }

  /** Scans the values (variables) that are defined outside this node
    */
  def freeVariables[G](
      node: Node[G],
      scope: CheckContext[G] = CheckContext[G](),
  ): Set[FreeVariable[G]] =
    flatFreeVariables(node, scope) ++
      node.subnodes.flatMap(freeVariables(_, node.enterCheckContext(scope)))
        .toSet
}
