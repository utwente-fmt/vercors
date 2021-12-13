package vct.col.util

import vct.col.ast.{Comparator, Declaration, Node}

import scala.collection.mutable

case object Compare {
  def equals[L, R](left: Node[L], right: Node[R]): Boolean = {
    Comparator.compare(left, right).isEmpty
  }

  def getIsomorphism[L, R](left: Node[L], right: Node[R]): Either[Seq[(Node[L], Node[R])], Map[Declaration[L], Declaration[R]]] = {
    val isomorphism = mutable.Map[Declaration[L], Declaration[R]]()
    val valueSet = mutable.Set[Declaration[R]]()
    val irreconcilableDiffs = mutable.ArrayBuffer[(Node[L], Node[R])]()
    Comparator.compare(left, right).foreach {
      case (left: Declaration[L], right: Declaration[R]) =>
        isomorphism.get(left) match {
          case None if !valueSet.contains(right) =>
            isomorphism(left) = right
            valueSet += right
          case Some(defn) if right == defn => // ok
          case _ => irreconcilableDiffs += ((left, right))
        }
      case diff => irreconcilableDiffs += diff
    }

    if(irreconcilableDiffs.isEmpty) Right(isomorphism.toMap)
    else Left(irreconcilableDiffs.toSeq)
  }

  def isIsomorphic[L, R](left: Node[L], right: Node[R]): Boolean =
    getIsomorphism(left, right).isRight
}
