package vct.rewrite.rasi

import vct.col.ast._

class VariableSelector[G](state: AbstractState[G]) {
  def distinguishing_variables(valuations: Set[Map[ConcreteVariable[G], UncertainValue]], expr: Option[Expr[G]]): Set[ConcreteVariable[G]] = {
    // val states: Set[AbstractState[G]] = valuations.map(v => state.with_new_valuation(v))
    // val vals = states.map(satisfying_valuations(_, expr))
    // TODO: Implement!
    Set()
  }

  private def satisfying_valuations(state: AbstractState[G], expr: Option[Expr[G]]): Map[ConcreteVariable[G], UncertainValue] = expr match {
    case None => Map.empty[ConcreteVariable[G], UncertainValue]
    case Some(e) =>
      if (!state.resolve_boolean_expression(e).can_be_false) Map.empty[ConcreteVariable[G], UncertainValue]
      else find_vals(state, e)
  }

  private def find_vals(state: AbstractState[G], expr: Expr[G]): Map[ConcreteVariable[G], UncertainValue] = expr match {
    case Deref(_, ref) => ???
    case Local(ref) => ???
    case SeqSubscript(seq, index) => ???
    case ArraySubscript(arr, index) => ???
    case Not(arg) => ???
    case And(left, right) => ???
    case Star(left, right) => ???
    case AmbiguousOr(left, right) => ???
    case Or(left, right) => ???
    case Implies(left, right) => ???
    case c: Comparison[_] => ???
  }
}
