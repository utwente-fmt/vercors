package vct.rewrite.rasi

import hre.io.RWFile
import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, RequiredName, SourceName}
import vct.col.util.{AstBuildHelpers, Substitute}

import java.io.Writer
import java.nio.file.Path

case object Utils {

  def extract_name(o: Origin): String = {
    o.find[SourceName].map(s => s.name).getOrElse(
      o.find[RequiredName].map(r => r.requiredName)
        .getOrElse(o.getPreferredName.get.snake)
    )
  }

  /** Returns the maximum absolute value between the two arguments.
    *
    * @param a
    *   Integer argument 1
    * @param b
    *   Integer argument 2
    * @return
    *   <code>max{|a|, |b|}</code>
    */
  def abs_max(a: Int, b: Int): Int = Seq(-a, a, -b, b).max

  /** Maximum product between either <code>a1</code> or <code>a2</code> and
    * either <code>b1</code> or <code>b2</code>.
    *
    * @param a1
    *   Alternative group 1
    * @param a2
    *   Alternative group 1
    * @param b1
    *   Alternative group 2
    * @param b2
    *   Alternative group 2
    * @return
    *   <code>max{a1*b1, a2*b1, a2*b1, a2*b2}</code>
    */
  def prod_max(a1: Int, a2: Int, b1: Int, b2: Int): Int =
    Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).max

  /** Minimum product between either <code>a1</code> or <code>a2</code> and
    * either <code>b1</code> or <code>b2</code>.
    *
    * @param a1
    *   Alternative group 1
    * @param a2
    *   Alternative group 1
    * @param b1
    *   Alternative group 2
    * @param b2
    *   Alternative group 2
    * @return
    *   <code>min{a1*b1, a2*b1, a2*b1, a2*b2}</code>
    */
  def prod_min(a1: Int, a2: Int, b1: Int, b2: Int): Int =
    Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).min

  /** Computes the cartesian product of an arbitrary number of input sequences.
    *
    * @param inputs
    *   A collection containing all inputs for the cartesian product
    * @tparam T
    *   Element type of the input sets
    * @return
    *   Set of ordered sequences with one element for each input set
    */
  def cartesian_product[T](inputs: Iterable[Set[T]]): Set[Seq[T]] = {
    if (inputs.isEmpty)
      Set.empty[Seq[T]]
    else if (inputs.size == 1)
      inputs.head.map(v => Seq(v))
    else
      inputs.head.flatMap(e => cartesian_product(inputs.tail).map(s => e +: s))
  }

  def extract_uncertainty[C, T](in: Map[C, Seq[T]]): Seq[Map[C, T]] = {
    if (in.isEmpty)
      Seq(Map.empty[C, T])
    else if (in.size == 1) {
      val (const: C, uncertain: Seq[T]) = in.head
      uncertain.map(v => Map.from(Seq(const -> v)))
    } else {
      val (const: C, uncertain: Seq[T]) = in.head
      extract_uncertainty(in.removed(const))
        .flatMap(m => uncertain.map(v => m + (const -> v)))
    }
  }

  /** Transforms a loop contract to an invariant, if possible.
    *
    * @param contract
    *   Loop contract
    * @return
    *   Loop invariant represented by <code>contract</code>
    */
  def loop_contract_to_expression[G](contract: LoopContract[G]): Expr[G] =
    contract match { case LoopInvariant(inv, _) => inv }

  /** Transforms an accounted predicate from a subroutine contract into a
    * boolean/resource expression.
    *
    * @param contract
    *   Subroutine contract
    * @return
    *   Boolean expression representing <code>contract</code>
    */
  def contract_to_expression[G](contract: AccountedPredicate[G]): Expr[G] =
    AstBuildHelpers.unfoldPredicate(contract)
      .reduce((e1, e2) => Star(e1, e2)(e1.o))

  /** Substitutes free variables in a boolean expression for other given
    * expressions according to the given map.
    *
    * @param cond
    *   Boolean condition, potentially containing free variables
    * @param args
    *   A map from free variables in <code>cond</code> to expressions to be
    *   substituted
    * @return
    *   The given condition with free variables replaced
    */
  def unify_expression[G](
      cond: Expr[G],
      args: Map[Variable[G], Expr[G]],
  ): Expr[G] =
    Substitute(args.map[Expr[G], Expr[G]] { case (v, e) =>
      Local[G](v.ref)(v.o) -> Old(e, None)(e.o)(e.o)
    }).dispatch(cond)

  /** Removes <code>\old</code> keywords from an expression.
    *
    * @param cond
    *   Expression to transform
    * @return
    *   Same expression as <code>cond</code> but with <code>\old</code>
    *   specifications removed
    */
  def remove_old[G](cond: Expr[G]): Expr[G] =
    Substitute(Map.from[Expr[G], Expr[G]](cond.collect {
      case o @ Old(expr, _) => o -> expr
    })).dispatch(cond)

  /** Determines whether an expression contains a reference to the global
    * invariant.
    *
    * @param node
    *   COL node containing an expression
    * @return
    *   <code>true</code> if <code>node</code> or one of its subnodes refers to
    *   the global invariant, <code>false</code> otherwise
    */
  def contains_global_invariant[G](node: Node[G]): Boolean =
    node match {
      case InstancePredicateApply(_, ref, _) =>
        if (ref.decl.o.getPreferredName.get.snake.equals("global_invariant"))
          true
        else
          contains_global_invariant(
            ref.decl.body.getOrElse(BooleanValue(value = true)(node.o))
          )
      case e: Expr[G] => e.subnodes.exists(n => contains_global_invariant(n))
      case _ => false
    }

  /** Transforms a valuation on resolvable variables to a valuation on a new
    * variable type.
    *
    * @param m
    *   Valuation on resolvable variables
    * @return
    *   Same valuation with all non-trackable variables removed
    */
  def cast_resolvable_map[G, S <: ResolvableVariable[
    G
  ], T <: ResolvableVariable[G], U <: UncertainValue](m: Map[S, U]): Map[T, U] =
    m.filter(t => t._1.isInstanceOf[T]).map(t => t._1.asInstanceOf[T] -> t._2)

  /** Computes the intersection of two variable valuations.
    *
    * @param v1
    *   Valuation
    * @param v2
    *   Valuation
    * @return
    *   The concatenation of <code>v1</code> and <code>v2</code>, with elements
    *   contained in both valuations mapped to the intersection of both
    */
  def val_intersect[G](
      v1: Map[FieldVariable[G], UncertainSingleValue],
      v2: Map[FieldVariable[G], UncertainSingleValue],
  ): Map[FieldVariable[G], UncertainSingleValue] =
    v1 ++ v2.map { case (k, v) =>
      k -> v.intersection(
        v1.getOrElse(k, UncertainSingleValue.uncertain_of(v.t[G]))
      ).asInstanceOf[UncertainSingleValue]
    }

  /** Extracts the component parts of a conjunction.
    *
    * @param conj
    *   Conjunction to be split
    * @return
    *   A sequence containing all subexpressions of the conjunction
    */
  def split_conjunction[G](conj: Expr[G]): Seq[Expr[G]] =
    conj match {
      case And(left, right) =>
        split_conjunction(left) ++ split_conjunction(right)
      case Star(left, right) =>
        split_conjunction(left) ++ split_conjunction(right)
      case _ => Seq(conj)
    }

  /** Unrolls a quantifier and substitutes in concrete values for known iterator
    * values as well as, if necessary, bounds that had to be narrowed down to a
    * single possibility.
    *
    * @param iterators
    *   A map from the quantifier iterator variables to their respective ranges,
    *   represented as a tuple of a lower and upper bound
    * @param body
    *   The body of the quantifier
    * @param substitutions
    *   A map of substitutions for known values other than the iterator
    *   variables
    * @param operator
    *   Combination operator for the individual instances of the quantifier,
    *   i.e. conjunction for universal and disjunction for existential
    *   quantifiers
    * @param default
    *   Neutral element of the <code>operator</code>
    * @return
    *   An expression that is logically equivalent to the quantifier, but
    *   unrolled with all instantiations
    */
  def replace_iterators_in_quantifier[G](
      iterators: Map[Variable[G], (Int, Int)],
      body: Expr[G],
      substitutions: Map[Expr[G], Expr[G]],
      operator: (Expr[G], Expr[G]) => Expr[G],
      default: Expr[G],
  ): Expr[G] = {
    val value_sets: Seq[Set[(Variable[G], Int)]] = iterators.toSeq
      .map(t => (t._2._1 to t._2._2).map(i => t._1 -> i).toSet)
    val value_maps: Seq[Map[Variable[G], Int]] =
      cartesian_product(value_sets).map(s => Map.from(s)).toSeq
    val expression_maps: Seq[Map[Expr[G], Expr[G]]] = value_maps.map(m =>
      m.map(t =>
        find_local_by_var(body, t._1).get -> IntegerValue(t._2)(body.o)
      )
    )
    val instantiations: Seq[Expr[G]] = expression_maps
      .map(m => Substitute(m ++ substitutions).dispatch(body))
    instantiations.fold(default)(operator)
  }

  private def find_local_by_var[G](
      body: Expr[G],
      variable: Variable[G],
  ): Option[Local[G]] =
    body.collectFirst { case l: Local[G] if l.ref.decl == variable => l }

  def fold_and[G](conds: Seq[Expr[G]]): Expr[G] =
    if (conds.length == 1)
      conds.head
    else
      conds.fold(BooleanValue[G](value = true)(origen))((e1, e2) =>
        And(e1, e2)(origen)
      )

  def fold_or[G](conds: Seq[Expr[G]]): Expr[G] =
    if (conds.length == 1)
      conds.head
    else
      conds.fold(BooleanValue[G](value = false)(origen))((e1, e2) =>
        Or(e1, e2)(origen)
      )

  def origen: Origin = Origin(Seq(LabelContext("RASI Generation")))

  /** Prints out the graph defined by the given states and edges to DOT format.
    *
    * @param states
    *   States of the graph, each representing an abstract state
    * @param edges
    *   Edges of the graph
    * @param out
    *   Path to the output file to write to
    */
  def print[G](
      states: Seq[AbstractState[G]],
      edges: Seq[(AbstractState[G], AbstractState[G])],
      out: Path,
  ): Unit = {
    val node_names: Map[AbstractState[G], String] = Map
      .from(states.zipWithIndex.map(t => (t._1, s"n${t._2}")))
    RWFile(out).write(w =>
      print_state_space(
        node_names,
        edges,
        w,
        states.head.to_expression(None).toInlineString.length > 100,
      )
    )
  }

  private def print_state_space[G](
      names: Map[AbstractState[G], String],
      edges: Seq[(AbstractState[G], AbstractState[G])],
      writer: Writer,
      shorten_labels: Boolean = false,
  ): Unit = {
    writer.append("digraph {\n")
    names.foreach(t =>
      writer.append(t._2).append(s"[label=${"\""}").append(
        if (shorten_labels)
          t._2
        else
          t._1.to_expression(None).toInlineString
      ).append(s"${"\""}];${if (shorten_labels)
          s" /* ${t._1.to_expression(None).toInlineString} */"
        else
          ""}\n")
    )
    edges.foreach(t =>
      writer.append(names(t._1)).append(" -> ").append(names(t._2))
        .append(";\n")
    )
    writer.append("}")
  }
}
