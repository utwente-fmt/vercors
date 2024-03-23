package vct.rewrite.rasi

import vct.col.ast._

class ConstraintSolver[G](state: AbstractState[G], vars: Set[_ <: ResolvableVariable[G]], is_contract: Boolean) {
  def resolve_assumption(expr: Expr[G]): Set[ConstraintMap[G]] = resolve(expr)

  private def resolve(expr: Expr[G], negate: Boolean = false): Set[ConstraintMap[G]] = expr match {
    // Consider boolean/separation logic operators
    case Not(arg) => this.resolve(arg, !negate)
    case AmbiguousOr(left, right) =>
      if (!negate) handle_or(left, right)
      else handle_and(left, right, neg_left = true, neg_right = true)
    case Star(left, right) =>
      if (!negate) handle_and(left, right)
      else handle_or(left, right, neg_left = true, neg_right = true)
    case And(left, right) =>
      if (!negate) handle_and(left, right)
      else handle_or(left, right, neg_left = true, neg_right = true)
    case Or(left, right) =>
      if (!negate) handle_or(left, right)
      else handle_and(left, right, neg_left = true, neg_right = true)
    case Implies(left, right) =>
      if (!negate) handle_or(left, right, neg_left = true)
      else handle_and(left, right, neg_right = true)
    // Atomic comparisons, if they contain a concrete variable, can actually affect the state
    case c: Comparison[G] => handle_update(c, negate)
    // Boolean variables could appear in the assumption without any comparison
    case e: Expr[G] if vars.exists(v => v.is(e, state)) => Set(ConstraintMap.from(get_var(e).get, UncertainBooleanValue.from(!negate)))
    // TODO: What other expressions could affect the state?
    case _ => Set(ConstraintMap.empty[G])
  }

  private def handle_and(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val left_is_possible: UncertainBooleanValue = state.resolve_boolean_expression(left, is_old = false, is_contract)
    val right_is_possible: UncertainBooleanValue = state.resolve_boolean_expression(right, is_old = false, is_contract)
    val possible_left: Boolean = (neg_left && left_is_possible.can_be_false) || (!neg_left && left_is_possible.can_be_true)
    val possible_right: Boolean = (neg_right && right_is_possible.can_be_false) || (!neg_right && right_is_possible.can_be_true)

    if (!possible_left || !possible_right) Set(ConstraintMap.impossible(vars))
    else {
      val left_constraints: Set[ConstraintMap[G]] = resolve(left, neg_left)
      val right_constraints: Set[ConstraintMap[G]] = resolve(right, neg_right)
      left_constraints.flatMap(m1 => right_constraints.map(m2 => m1 ++ m2))
    }
  }

  private def handle_or(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val left_is_possible: UncertainBooleanValue = state.resolve_boolean_expression(left, is_old = false, is_contract)
    val right_is_possible: UncertainBooleanValue = state.resolve_boolean_expression(right, is_old = false, is_contract)
    val possible_left: Boolean = (neg_left && left_is_possible.can_be_false) || (!neg_left && left_is_possible.can_be_true)
    val possible_right: Boolean = (neg_right && right_is_possible.can_be_false) || (!neg_right && right_is_possible.can_be_true)

    if (!possible_left && !possible_right) Set(ConstraintMap.impossible(vars))
    else if (!possible_left) resolve(right, neg_right)
    else if (!possible_right) resolve(left, neg_left)
    else {
      // Both branches are viable
      val pure_left: Boolean = is_pure(left)
      val pure_right: Boolean = is_pure(right)
      // If one side is pure and the other has an effect on the state, treat this "or" as an implication. If both sides
      // update the state, treat it as a split in the state space instead
      if (pure_left && pure_right) Set(ConstraintMap.empty[G])
      else if (pure_left) handle_implies(left, right, !neg_left, neg_right)
      else if (pure_right) handle_implies(right, left, !neg_right, neg_left)
      else resolve(left, neg_left) ++ resolve(right, neg_right)
    }
  }

  private def handle_implies(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val resolve_left: UncertainBooleanValue = state.resolve_boolean_expression(left, is_old = false, is_contract)
    var res: Set[ConstraintMap[G]] = Set()
    if (neg_left && resolve_left.can_be_false || (!neg_left) && resolve_left.can_be_true) res = res ++ resolve(right, neg_right)
    if (neg_left && resolve_left.can_be_true || (!neg_left) && resolve_left.can_be_false) res = res ++ Set(ConstraintMap.empty[G])
    res
  }

  private def handle_update(comp: Comparison[G], negate: Boolean): Set[ConstraintMap[G]] = {
    val pure_left = is_pure(comp.left)
    val pure_right = is_pure(comp.right)
    if (pure_left == pure_right) return Set(ConstraintMap.empty[G])
    // Now, exactly one side is pure, and the other contains a concrete variable
    // Resolve the variable and the other side of the equation    TODO: Reduce the side with the variable if it contains anything else as well
    if (vars.exists(v => v.is(if (pure_left) comp.right else comp.left, state))) handle_single_update(comp, pure_left, negate)
    else handle_collection_update(comp, pure_left, negate)
  }

  private def is_pure(node: Node[G]): Boolean = node match {
    // The old value of a variable is always pure, since it cannot be updated
    case _: Old[_] => true
    case e: UnExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: BinExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: Expr[_] => if (vars.exists(v => v.is_contained_by(e, state))) false else e.subnodes.forall(n => is_pure(n))
    case _ => true
  }

  private def handle_single_update(comp: Comparison[G], pure_left: Boolean, negate: Boolean): Set[ConstraintMap[G]] = {
    val variable: ResolvableVariable[G] = if (pure_left) get_var(comp.right).get else get_var(comp.left).get
    val value: UncertainValue = if (pure_left) state.resolve_expression(comp.left)
                                else state.resolve_expression(comp.right)

    comp match {
      case _: Eq[_] => Set(if (!negate) ConstraintMap.from(variable, value) else ConstraintMap.from(variable, value.complement()))
      case _: Neq[_] => Set(if (!negate) ConstraintMap.from(variable, value.complement()) else ConstraintMap.from(variable, value))
      case AmbiguousGreater(_, _) | Greater(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, negate)
      case AmbiguousGreaterEq(_, _) | GreaterEq(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, !negate)
      case AmbiguousLessEq(_, _) | LessEq(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, !negate)
      case AmbiguousLess(_, _) | Less(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, negate)
    }
  }

  private def limit_variable(v: ResolvableVariable[G], b: UncertainIntegerValue, var_greater: Boolean, can_be_equal: Boolean): Set[ConstraintMap[G]] = {
    if (var_greater) {
      if (can_be_equal) Set(ConstraintMap.from(v, b.above_eq()))
      else Set(ConstraintMap.from(v, b.above()))
    }
    else {
      if (can_be_equal) Set(ConstraintMap.from(v, b.below_eq()))
      else Set(ConstraintMap.from(v, b.below()))
    }
  }

  private def handle_collection_update(comp: Comparison[G], pure_left: Boolean, negate: Boolean): Set[ConstraintMap[G]] = {
    val variable: Expr[G] = if (pure_left) comp.right else comp.left
    val value: UncertainSequence = state.resolve_collection_expression(if (pure_left) comp.left else comp.right)
    val affected: Set[(IndexedVariable[G], Int)] = get_contained_vars(variable)

    comp match {
      case _: Eq[_] if !negate => Set(ConstraintMap.from_cons(affected.map(t => t._1 -> value.get(t._2))))
      case _: Neq[_] if negate => Set(ConstraintMap.from_cons(affected.map(t => t._1 -> value.get(t._2))))
      case _ => throw new IllegalArgumentException(s"The operator ${comp.toInlineString} is not supported for collections")
    }
  }

  private def get_contained_vars(variable: Expr[G]): Set[(IndexedVariable[G], Int)] = variable match {
    case LiteralSeq(_, _) | UntypedLiteralSeq(_) | Old(_, _) => Set()
    case d: Deref[_] =>
      val variables: Set[IndexedVariable[G]] = vars.filter(v => v.is_contained_by(d, state)).collect{ case v: IndexedVariable[G] => v }
      variables.map(v => (v, v.i))
    case Take(xs, count) => state.resolve_integer_expression(count).try_to_resolve() match {
      case None => ???
      case Some(i) => get_contained_vars(xs).filter(t => t._2 < i)
    }
    case Drop(xs, count) => state.resolve_integer_expression(count).try_to_resolve() match {
      case None => ???
      case Some(i) => get_contained_vars(xs).filter(t => t._2 >= i).map(t => (t._1, t._2 - i))
    }
    case Slice(xs, from, to) => state.resolve_integer_expression(from).try_to_resolve() match {
      case None => ???
      case Some(l) => state.resolve_integer_expression(to).try_to_resolve() match {
        case None => ???
        case Some(u) => get_contained_vars(xs).filter(t => l <= t._2 && t._2 < u).map(t => (t._1, t._2 - l))
      }
    }
    case Concat(left, right) => state.resolve_collection_expression(left).len.try_to_resolve() match {
      case None => ???
      case Some(i) => get_contained_vars(left) ++ get_contained_vars(right).map(t => (t._1, t._2 + i))
    }
    case AmbiguousPlus(left, right) => state.resolve_collection_expression(left).len.try_to_resolve() match {
      case None => ???
      case Some(i) => get_contained_vars(left) ++ get_contained_vars(right).map(t => (t._1, t._2 + i))
    }
    case Select(cond, ift, iff) =>
      val condition = state.resolve_boolean_expression(cond)
      if (condition.can_be_true && !condition.can_be_false) get_contained_vars(ift)
      else if (condition.can_be_false && !condition.can_be_true) get_contained_vars(iff)
      else ???
  }

  private def get_var(expr: Expr[G]): Option[ResolvableVariable[G]] = vars.collectFirst{ case v: ResolvableVariable[G] if v.is(expr, state) => v }
}
