package vct.col.ast

case object JavaAnnotationData {
  final case class BipTransition[G](name: String, source: String, target: String, guard: Either[String, (Expr[G], Expr[G])]) extends JavaAnnotationData[G]
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]
  final case class BipComponentType[G](name: String, initial: String) extends JavaAnnotationData[G]
  final case class BipStatePredicate[G](state: String, expr: Expr[G]) extends JavaAnnotationData[G]
}

sealed trait JavaAnnotationData[G] {

}
