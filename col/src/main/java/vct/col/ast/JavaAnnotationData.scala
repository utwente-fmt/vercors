package vct.col.ast

case object JavaAnnotationData {
  final case class BipTransitionData[G](name: String, source: String, target: String, guard: Either[String, (Expr[G], Expr[G])]) extends JavaAnnotationData[G]
}

sealed trait JavaAnnotationData[G] {

}
