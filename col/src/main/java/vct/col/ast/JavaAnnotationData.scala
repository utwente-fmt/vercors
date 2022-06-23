package vct.col.ast

import vct.col.ref.Ref
import vct.col.resolve.{RefJavaBipStatePredicate, RefJavaMethod}

case object JavaAnnotationData {
  final case class BipTransition[G](name: String, source: RefJavaBipStatePredicate[G], target: RefJavaBipStatePredicate[G],
                                    guard: Either[RefJavaMethod[G], (Expr[G], Expr[G])]) extends JavaAnnotationData[G]
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]
  final case class BipComponentType[G](name: String, initial: RefJavaBipStatePredicate[G]) extends JavaAnnotationData[G]
  final case class BipStatePredicate[G](name: String, expr: Expr[G]) extends JavaAnnotationData[G]
}

sealed trait JavaAnnotationData[G] {

}
