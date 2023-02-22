package vct.options.types

sealed trait FloatMode

case object FloatMode extends ReadEnum[FloatMode] {
  override val options: Map[String, FloatMode] = Map(
    "rational" -> Rational,
    "z3" -> Z3,
  )

  case object Rational extends FloatMode
  case object Z3 extends FloatMode
}