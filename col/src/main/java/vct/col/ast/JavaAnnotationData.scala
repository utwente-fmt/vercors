package vct.col.ast

import vct.col.resolve.JavaBipStatePredicateTarget

case object JavaAnnotationData {
  final case class BipTransition[G](name: String, source: JavaBipStatePredicateTarget[G], target: JavaBipStatePredicateTarget[G],
                                    guard: Option[JavaMethod[G]], requires: Expr[G], ensures: Expr[G]) extends JavaAnnotationData[G]
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]
  final case class BipComponentType[G](name: String, initial: JavaBipStatePredicateTarget[G]) extends JavaAnnotationData[G]
  final case class BipStatePredicate[G](name: String, expr: Expr[G]) extends JavaAnnotationData[G]
  final case class BipData[G](name: String) extends JavaAnnotationData[G]
  final case class BipGuard[G](name: String) extends JavaAnnotationData[G]
}

sealed trait JavaAnnotationData[G] {

}
