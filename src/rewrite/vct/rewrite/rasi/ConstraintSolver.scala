package vct.rewrite.rasi

import vct.col.ast._

class ConstraintSolver[G](
    state: AbstractState[G],
    vars: Set[_ <: ResolvableVariable[G]],
    is_contract: Boolean,
) {

  /** Resolves the constraints necessary to make the given boolean expression
    * true. If there are multiple disjoint possibilities to make it true,
    * returns one constraint for each possibility separately.
    *
    * @param expr
    *   Boolean formula to be resolved to a constraint
    * @return
    *   A set of constraint maps mapping variables to possible values
    */
  def resolve_assumption(expr: Expr[G]): Set[ConstraintMap[G]] = resolve(expr)

  private def resolve(
      expr: Expr[G],
      negate: Boolean = false,
  ): Set[ConstraintMap[G]] =
    expr match {
      // Consider boolean/separation logic operators
      case Not(arg) => this.resolve(arg, !negate)
      case AmbiguousOr(left, right) =>
        if (!negate)
          handle_or(left, right)
        else
          handle_and(left, right, neg_left = true, neg_right = true)
      case Star(left, right) =>
        if (!negate)
          handle_and(left, right)
        else
          handle_or(left, right, neg_left = true, neg_right = true)
      case And(left, right) =>
        if (!negate)
          handle_and(left, right)
        else
          handle_or(left, right, neg_left = true, neg_right = true)
      case Or(left, right) =>
        if (!negate)
          handle_or(left, right)
        else
          handle_and(left, right, neg_left = true, neg_right = true)
      case Implies(left, right) =>
        if (!negate)
          handle_or(left, right, neg_left = true)
        else
          handle_and(left, right, neg_right = true)
      // Atomic comparisons, if they contain a concrete variable, can actually affect the state
      case c: Comparison[G] => handle_update(c, negate)
      // Boolean variables could appear in the assumption without any comparison
      case e: Expr[G] if vars.exists(v => v.is(e, state)) =>
        Set(
          ConstraintMap
            .from(get_var(e).get, UncertainBooleanValue.from(!negate))
        )
      // If there are variables in the state that are not tracked that make this valuation impossible, make an impossible constraint
      case e: Expr[G] if state.valuations.exists(v => v._1.is(e, state)) =>
        val state_entry: Option[(ConcreteVariable[G], UncertainValue)] = state
          .valuations.collectFirst { case (k, v) if k.is(e, state) => (k, v) }
        if (
          state_entry.get._2.can_be_equal(UncertainBooleanValue.from(!negate))
        )
          Set(ConstraintMap.empty[G])
        else
          Set(ConstraintMap.impossible(Set(state_entry.get._1)))
      // TODO: What other expressions could affect the state?
      case _ => Set(ConstraintMap.empty[G])
    }

  private def handle_and(
      left: Expr[G],
      right: Expr[G],
      neg_left: Boolean = false,
      neg_right: Boolean = false,
  ): Set[ConstraintMap[G]] = {
    val left_is_possible: UncertainBooleanValue = state
      .resolve_boolean_expression(left, is_old = false, is_contract)
    val right_is_possible: UncertainBooleanValue = state
      .resolve_boolean_expression(right, is_old = false, is_contract)
    val possible_left: Boolean = (neg_left && left_is_possible.can_be_false) ||
      (!neg_left && left_is_possible.can_be_true)
    val possible_right: Boolean =
      (neg_right && right_is_possible.can_be_false) ||
        (!neg_right && right_is_possible.can_be_true)

    if (!possible_left || !possible_right)
      Set(ConstraintMap.impossible(vars))
    else {
      val left_constraints: Set[ConstraintMap[G]] = resolve(left, neg_left)
      val right_constraints: Set[ConstraintMap[G]] = resolve(right, neg_right)
      left_constraints.flatMap(m1 => right_constraints.map(m2 => m1 && m2))
    }
  }

  private def handle_or(
      left: Expr[G],
      right: Expr[G],
      neg_left: Boolean = false,
      neg_right: Boolean = false,
  ): Set[ConstraintMap[G]] = {
    val left_is_possible: UncertainBooleanValue = state
      .resolve_boolean_expression(left, is_old = false, is_contract)
    val right_is_possible: UncertainBooleanValue = state
      .resolve_boolean_expression(right, is_old = false, is_contract)
    val possible_left: Boolean = (neg_left && left_is_possible.can_be_false) ||
      (!neg_left && left_is_possible.can_be_true)
    val possible_right: Boolean =
      (neg_right && right_is_possible.can_be_false) ||
        (!neg_right && right_is_possible.can_be_true)

    if (!possible_left && !possible_right)
      Set(ConstraintMap.impossible(vars))
    else if (!possible_left)
      resolve(right, neg_right)
    else if (!possible_right)
      resolve(left, neg_left)
    else {
      // Both branches are viable
      val pure_left: Boolean = is_pure(left)
      val pure_right: Boolean = is_pure(right)
      // If one side is pure and the other has an effect on the state, treat this "or" as an implication. If both sides
      // update the state, treat it as a split in the state space instead
      if (pure_left && pure_right)
        Set(ConstraintMap.empty[G])
      else if (pure_left)
        handle_implies(left, right, !neg_left, neg_right)
      else if (pure_right)
        handle_implies(right, left, !neg_right, neg_left)
      else
        resolve(left, neg_left) ++ resolve(right, neg_right)
    }
  }

  private def handle_implies(
      left: Expr[G],
      right: Expr[G],
      neg_left: Boolean = false,
      neg_right: Boolean = false,
  ): Set[ConstraintMap[G]] = {
    val resolve_left: UncertainBooleanValue = state
      .resolve_boolean_expression(left, is_old = false, is_contract)
    var res: Set[ConstraintMap[G]] = Set()
    if (
      neg_left && resolve_left.can_be_false || (!neg_left) &&
      resolve_left.can_be_true
    ) {
      val constraints: Set[ConstraintMap[G]] = resolve(right, neg_right)
      val variables = vars.asInstanceOf[Set[ResolvableVariable[G]]]
        .diff(constraints.flatMap(c => c.resolve.keySet))
      val path_condition: ConstraintMap[G] = resolve_path_condition(
        if (neg_left)
          Not(left)(left.o)
        else
          left,
        variables,
      )
      res = res ++ constraints.map(m => m && path_condition)
    }
    if (
      neg_left && resolve_left.can_be_true || (!neg_left) &&
      resolve_left.can_be_false
    ) {
      val path_condition: ConstraintMap[G] = ConstraintMap
        .empty[G] // TODO: resolve_path_condition(if (neg_left) left else Not(left)(left.o), vars)
      res = res + path_condition
    }
    res
  }

  private def handle_update(
      comp: Comparison[G],
      negate: Boolean,
  ): Set[ConstraintMap[G]] = {
    val pure_left = is_pure(comp.left)
    val pure_right = is_pure(comp.right)
    if (pure_left == pure_right)
      return Set(ConstraintMap.empty[G])
    // Now, exactly one side is pure, and the other contains a concrete variable
    // Resolve the variable and the other side of the equation
    comp.left.t match {
      case _: TSeq[_] | _: TArray[_] =>
        handle_collection_update(comp, pure_left, negate)
      case _ => handle_single_update(comp, pure_left, negate)
    }
  }

  private def is_pure(node: Node[G]): Boolean =
    node match {
      // The old value of a variable is always pure, since it cannot be updated
      case _: Old[_] => true
      case e: UnExpr[_] => e.subnodes.forall(n => is_pure(n))
      case e: BinExpr[_] => e.subnodes.forall(n => is_pure(n))
      case e: Expr[_] =>
        if (vars.exists(v => v.is_contained_by(e, state)))
          false
        else
          e.subnodes.forall(n => is_pure(n))
      case _ => true
    }

  private def handle_single_update(
      comp: Comparison[G],
      pure_left: Boolean,
      negate: Boolean,
  ): Set[ConstraintMap[G]] = {
    val expr: Expr[G] =
      if (pure_left)
        comp.right
      else
        comp.left
    val value: UncertainValue =
      if (pure_left)
        state.resolve_expression(comp.left)
      else
        state.resolve_expression(comp.right)

    comp match {
      case _: Eq[_] | _: AmbiguousEq[_] =>
        Set(
          if (!negate)
            expr_equals(expr, value)
          else
            expr_equals(expr, value.complement())
        )
      case _: Neq[_] | _: AmbiguousNeq[_] =>
        Set(
          if (!negate)
            expr_equals(expr, value.complement())
          else
            expr_equals(expr, value)
        )
      case AmbiguousGreater(_, _) | Greater(_, _) =>
        limit_variable(
          expr,
          value.asInstanceOf[UncertainIntegerValue],
          pure_left == negate,
          negate,
        )
      case AmbiguousGreaterEq(_, _) | GreaterEq(_, _) =>
        limit_variable(
          expr,
          value.asInstanceOf[UncertainIntegerValue],
          pure_left == negate,
          !negate,
        )
      case AmbiguousLessEq(_, _) | LessEq(_, _) =>
        limit_variable(
          expr,
          value.asInstanceOf[UncertainIntegerValue],
          pure_left != negate,
          !negate,
        )
      case AmbiguousLess(_, _) | Less(_, _) =>
        limit_variable(
          expr,
          value.asInstanceOf[UncertainIntegerValue],
          pure_left != negate,
          negate,
        )
    }
  }

  private def limit_variable(
      expr: Expr[G],
      b: UncertainIntegerValue,
      var_greater: Boolean,
      can_be_equal: Boolean,
  ): Set[ConstraintMap[G]] = {
    if (var_greater) {
      if (can_be_equal)
        Set(expr_equals(expr, b.above_eq()))
      else
        Set(expr_equals(expr, b.above()))
    } else {
      if (can_be_equal)
        Set(expr_equals(expr, b.below_eq()))
      else
        Set(expr_equals(expr, b.below()))
    }
  }

  private def expr_equals(
      expr: Expr[G],
      value: UncertainValue,
  ): ConstraintMap[G] = {
    if (get_var(expr).nonEmpty)
      ConstraintMap.from(get_var(expr).get, value)
    else
      expr match {
        case UMinus(arg) =>
          expr_equals(arg, -value.asInstanceOf[UncertainIntegerValue])
        case AmbiguousPlus(left, right) =>
          plus_equals(left, right, value.asInstanceOf[UncertainIntegerValue])
        case Plus(left, right) =>
          plus_equals(left, right, value.asInstanceOf[UncertainIntegerValue])
        case AmbiguousMinus(left, right) =>
          minus_equals(left, right, value.asInstanceOf[UncertainIntegerValue])
        case Minus(left, right) =>
          minus_equals(left, right, value.asInstanceOf[UncertainIntegerValue])
        case _ => ConstraintMap.empty[G]
      }
  }

  private def plus_equals(
      left: Expr[G],
      right: Expr[G],
      value: UncertainIntegerValue,
  ): ConstraintMap[G] =
    expr_equals(left, value - state.resolve_integer_expression(right)) &&
      expr_equals(right, value - state.resolve_integer_expression(left))

  private def minus_equals(
      left: Expr[G],
      right: Expr[G],
      value: UncertainIntegerValue,
  ): ConstraintMap[G] = {
    expr_equals(left, value + state.resolve_integer_expression(right)) &&
    expr_equals(right, state.resolve_integer_expression(right) - value)
  }

  private def handle_collection_update(
      comp: Comparison[G],
      pure_left: Boolean,
      negate: Boolean,
  ): Set[ConstraintMap[G]] = {
    val variable: Expr[G] =
      if (pure_left)
        comp.right
      else
        comp.left
    val value: UncertainSequence = state.resolve_collection_expression(
      if (pure_left)
        comp.left
      else
        comp.right
    )
    val indices: Set[(IndexedVariable[G], Int)] = get_contained_indices(
      variable
    )
    val size_vars: Set[SizeVariable[G]] = get_size_vars(variable)

    val index_update_map: Set[(ResolvableVariable[G], UncertainValue)] = indices
      .map(t => (t._1, value.get(t._2)))
    val size_update_map: Set[(ResolvableVariable[G], UncertainValue)] =
      size_vars.map(v => (v, value.len))
    val update_map = index_update_map ++ size_update_map

    comp match {
      case _: Eq[_] | _: AmbiguousEq[_] if !negate =>
        Set(ConstraintMap.from_cons(update_map))
      case _: Neq[_] | _: AmbiguousNeq[_] if negate =>
        Set(ConstraintMap.from_cons(update_map))
      case _ =>
        throw new IllegalArgumentException(
          s"The operator ${comp.toInlineString} is not supported for collections"
        )
    }
  }

  private def get_contained_indices(
      variable: Expr[G]
  ): Set[(IndexedVariable[G], Int)] =
    variable match {
      case LiteralSeq(_, _) | UntypedLiteralSeq(_) | Old(_, _) => Set()
      case d: Deref[_] =>
        val variables: Set[IndexedVariable[G]] = vars
          .filter(v => v.is_contained_by(d, state)).collect {
            case v: IndexedVariable[G] => v
          }
        variables.map(v => (v, v.i))
      case Take(xs, count) =>
        state.resolve_integer_expression(count).try_to_resolve() match {
          case None => ???
          case Some(i) => get_contained_indices(xs).filter(t => t._2 < i)
        }
      case Drop(xs, count) =>
        state.resolve_integer_expression(count).try_to_resolve() match {
          case None => ???
          case Some(i) =>
            get_contained_indices(xs).filter(t => t._2 >= i)
              .map(t => (t._1, t._2 - i))
        }
      case Slice(xs, from, to) =>
        state.resolve_integer_expression(from).try_to_resolve() match {
          case None => ???
          case Some(l) =>
            state.resolve_integer_expression(to).try_to_resolve() match {
              case None => ???
              case Some(u) =>
                get_contained_indices(xs).filter(t => l <= t._2 && t._2 < u)
                  .map(t => (t._1, t._2 - l))
            }
        }
      case Concat(left, right) =>
        state.resolve_collection_expression(left).len.try_to_resolve() match {
          case None => ???
          case Some(i) =>
            get_contained_indices(left) ++ get_contained_indices(right)
              .map(t => (t._1, t._2 + i))
        }
      case AmbiguousPlus(left, right) =>
        state.resolve_collection_expression(left).len.try_to_resolve() match {
          case None => ???
          case Some(i) =>
            get_contained_indices(left) ++ get_contained_indices(right)
              .map(t => (t._1, t._2 + i))
        }
      case Select(cond, ift, iff) =>
        val condition = state.resolve_boolean_expression(cond)
        if (condition.can_be_true && !condition.can_be_false)
          get_contained_indices(ift)
        else if (condition.can_be_false && !condition.can_be_true)
          get_contained_indices(iff)
        else
          ???
    }

  private def resolve_path_condition(
      cond: Expr[G],
      variables: Set[_ <: ResolvableVariable[G]],
  ): ConstraintMap[G] = {
    new ConstraintSolver(state, variables, false)
      .resolve_assumption(Utils.remove_old(cond)).reduce((m1, m2) => m1 || m2)
  }

  private def get_size_vars(variable: Expr[G]): Set[SizeVariable[G]] =
    vars.filter(v => v.is_contained_by(variable, state)).collect {
      case v: SizeVariable[G] => v
    }

  private def get_var(expr: Expr[G]): Option[ResolvableVariable[G]] =
    vars.collectFirst {
      case v: ResolvableVariable[G] if v.is(expr, state) => v
    }
}
