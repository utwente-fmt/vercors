package vct.col.ast

import vct.col.resolve.JavaBipStatePredicateTarget

case object JavaAnnotationData {
  final case class BipTransition[G](name: String, source: JavaBipStatePredicateTarget[G], target: JavaBipStatePredicateTarget[G],
                                    guard: Either[JavaMethod[G], (Expr[G], Expr[G])]) extends JavaAnnotationData[G]
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]
  final case class BipComponentType[G](name: String, initial: JavaBipStatePredicateTarget[G]) extends JavaAnnotationData[G]
  final case class BipStatePredicate[G](name: String, expr: Expr[G]) extends JavaAnnotationData[G]
}

sealed trait JavaAnnotationData[G] {

}
