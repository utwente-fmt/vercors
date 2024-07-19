package vct.rewrite.rasi

import hre.io.RWFile
import vct.col.ast._
import vct.col.util.{AstBuildHelpers, Substitute}

import java.io.Writer
import java.nio.file.Path

case object Utils {

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

  /** Transforms a valuation on resolvable variables to a valuation on concrete
    * variables.
    *
    * @param m
    *   Valuation on resolvable variables
    * @return
    *   Same valuation with all non-trackable variables removed
    */
  def resolvable_to_concrete[G](
      m: Map[ResolvableVariable[G], UncertainValue]
  ): Map[ConcreteVariable[G], UncertainValue] =
    m.filter(t => t._1.isInstanceOf[ConcreteVariable[G]])
      .map(t => t._1.asInstanceOf[ConcreteVariable[G]] -> t._2)

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
      v1: Map[ConcreteVariable[G], UncertainValue],
      v2: Map[ConcreteVariable[G], UncertainValue],
  ): Map[ConcreteVariable[G], UncertainValue] =
    v1 ++ v2.map { case (k, v) =>
      k -> v.intersection(v1.getOrElse(k, UncertainValue.uncertain_of(v.t[G])))
    }

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
