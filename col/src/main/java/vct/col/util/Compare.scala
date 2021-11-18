package vct.col.util

import vct.col.ast.{Comparator, Declaration, Node}

import scala.collection.mutable

case object Compare {
  def equals(left: Node, right: Node): Boolean = {
    Comparator.compare(left, right).isEmpty
  }

  def getIsomorphism(left: Node, right: Node): Either[Seq[(Node, Node)], Map[Declaration, Declaration]] = {
    val isomorphism = mutable.Map[Declaration, Declaration]()
    val valueSet = mutable.Set[Declaration]()
    val irreconcilableDiffs = mutable.ArrayBuffer[(Node, Node)]()
    Comparator.compare(left, right).foreach {
      case (left: Declaration, right: Declaration) =>
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

  def isIsomorphic(left: Node, right: Node): Boolean =
    getIsomorphism(left, right).isRight
}
