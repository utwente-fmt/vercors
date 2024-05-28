package vct.col.compare

import vct.col.ast.{Declaration, Node}

import scala.collection.mutable

case object Compare {
  def compare[L, R](left: Node[L], right: Node[R])(toNormalForm: PartialFunction[Node[L], Node[L]]): LazyList[CompareResult[L, R]] =
    underEquivalence(left.compare(right))(toNormalForm)

  def underEquivalence[L, R](diffs: LazyList[CompareResult[L, R]])(toNormalForm: PartialFunction[Node[L], Node[L]]): LazyList[CompareResult[L, R]] =
    diffs.flatMap {
      case diff @ StructuralDifference(left, right) =>
        (toNormalForm.lift(left), toNormalForm.lift(right.asInstanceOf[Node[L]]).asInstanceOf[Option[Node[R]]]) match {
          case (None, None) => Seq(diff)
          case (maybeLeft, maybeRight) =>
            compare(maybeLeft.getOrElse(left), maybeRight.getOrElse(right))(toNormalForm)
        }
      case otherDiff => Seq(otherDiff)
    }

  def equals[L, R](left: Node[L], right: Node[R]): Boolean =
    left.compare(right).isEmpty

  def getIsomorphism[L, R](left: Node[L], right: Node[R], matchFreeVariables: Boolean = false): Either[Seq[(Node[L], Node[R])], Map[Declaration[L], Declaration[R]]] = {
    val isomorphism = mutable.Map[Declaration[L], Declaration[R]]()
    val valueSet = mutable.Set[Declaration[R]]()
    val irreconcilableDiffs = mutable.ArrayBuffer[(Node[L], Node[R])]()
    val boundVariables = mutable.Set[Declaration[L]]()

    def define(left: Declaration[L], right: Declaration[R], mustMatch: Boolean): Unit = {
      if (mustMatch && left != right) {
        irreconcilableDiffs += ((left, right))
        return;
      }
      isomorphism.get(left) match {
        case None if !valueSet.contains(right) =>
          isomorphism(left) = right
          valueSet += right
        case Some(defn) if right == defn => // ok
        case _ => irreconcilableDiffs += ((left, right))
      }
    }

    left.compare(right).foreach {
      case MatchingReference(left, right) => define(left, right, mustMatch = matchFreeVariables && !boundVariables.contains(left))
      case MatchingDeclaration(left, right) =>
        boundVariables.add(left)
        define(left, right, mustMatch = false)
      case StructuralDifference(left, right) =>
        irreconcilableDiffs += ((left, right))
    }

    if(irreconcilableDiffs.isEmpty) Right(isomorphism.toMap)
    else Left(irreconcilableDiffs.toSeq)
  }

  def isIsomorphic[L, R](left: Node[L], right: Node[R], matchFreeVariables: Boolean = false): Boolean =
    getIsomorphism(left, right, matchFreeVariables).isRight
}
