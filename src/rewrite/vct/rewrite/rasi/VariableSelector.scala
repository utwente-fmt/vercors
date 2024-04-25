package vct.rewrite.rasi

import vct.col.ast._

class VariableSelector[G](state: AbstractState[G]) {
  def distinguishing_variables(valuations: Seq[Map[ConcreteVariable[G], UncertainValue]], expr: Option[Expr[G]]): Set[ConcreteVariable[G]] = ???

  def satisfying_valuations(state: AbstractState[G], expr: Option[Expr[G]]): Map[ConcreteVariable[G], UncertainValue] = expr match {
    case None => Map.empty[ConcreteVariable[G], UncertainValue]
    case Some(e) =>
      if (!state.resolve_boolean_expression(e).can_be_false) Map.empty[ConcreteVariable[G], UncertainValue]
      else e match {
        case And(left, right) => ???
        case Star(left, right) => ???
        case Or(left, right) => ???
      }
  }
}
