package vct.rewrite.rasi

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  Deref,
  Expr,
  InstanceField,
  InstanceMethod,
  InstancePredicate,
  Null,
  Or,
}
import vct.col.origin.Origin
import vct.rewrite.cfg.{CFGEntry, CFGGenerator}

import java.nio.file.Path
import scala.collection.immutable.HashMap
import scala.collection.mutable

class RASIGenerator[G] extends LazyLogging {
  private val found_states: mutable.ArrayBuffer[AbstractState[G]] = mutable
    .ArrayBuffer()
  private val found_edges: mutable.ArrayBuffer[RASIEdge[G]] = mutable
    .ArrayBuffer()
  private val current_branches: mutable.ArrayBuffer[AbstractState[G]] = mutable
    .ArrayBuffer()

  def execute(
      entry_point: InstanceMethod[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
  ): Expr[G] =
    generate_rasi(
      CFGGenerator().generate(entry_point),
      vars,
      parameter_invariant,
    )

  def test(
      entry_point: InstanceMethod[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
      out_path: Path,
  ): Unit =
    print_state_space(
      CFGGenerator().generate(entry_point),
      vars,
      parameter_invariant,
      out_path,
    )

  private def generate_rasi(
      node: CFGEntry[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
  ): Expr[G] = {
    explore(node, vars, parameter_invariant)
    val distinct_states = found_states.distinctBy(s => s.valuations)
    logger.debug(s"${distinct_states.size} distinct states found")
    distinct_states.map(s => s.to_expression)
      .reduce((e1, e2) => Or(e1, e2)(e1.o))
  }

  private def print_state_space(
      node: CFGEntry[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
      out_path: Path,
  ): Unit = {
    explore(node, vars, parameter_invariant)
    val (ns, es) = reduce_redundant_states()
    logger.debug(s"${ns.size} distinct states found")
    Utils.print(ns, es, out_path)
  }

  private def explore(
      node: CFGEntry[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
  ): Unit = {
    logger.info("Starting RASI generation")
    val global_start_time: Long = System.nanoTime()

    var considered_variables: Set[ConcreteVariable[G]] = vars

    var generation_start_time: Long = reset(
      node,
      considered_variables,
      parameter_invariant,
    )
    var last_measurement_time: Long = generation_start_time
    var initial_state: AbstractState[G] = current_branches.head
    var i = 0

    while (current_branches.nonEmpty) {
      val curr: AbstractState[G] = current_branches.head
      current_branches -= curr

      val successor: RASISuccessor[G] = curr.successors()

      val relevant_variables: Set[ConcreteVariable[G]] = successor
        .deciding_variables.filter(v => is_relevant_variable(v))
      if (relevant_variables.nonEmpty) {
        val time: Long =
          (System.nanoTime() - generation_start_time) / 1_000_000L
        logger.info(
          s"Found relevant new variables; abort generation [$i iterations in ${time}ms]"
        )
        val found_vars = relevant_variables.toSeq
          .sortWith((v1, v2) => v1.compare(v2))
          .map(v => v.to_expression.toInlineString)
        logger.debug(s"Variables found: $found_vars")

        considered_variables ++= relevant_variables

        generation_start_time = reset(
          node,
          considered_variables,
          parameter_invariant,
        )
        last_measurement_time = generation_start_time
        initial_state = current_branches.head
        i = 0
      } else {
        found_edges.addAll(successor.edges(curr))
        successor.successors.foreach(s =>
          if (!found_states.contains(s)) {
            found_states += s;
            current_branches += s
          }
        )
        i = i + 1
        if (System.nanoTime() - last_measurement_time > 1_000_000_000L) {
          last_measurement_time = System.nanoTime()
          val time =
            (last_measurement_time - generation_start_time) / 1_000_000L
          logger.debug(s"[Runtime ${time}ms] Iteration $i: ${found_states
              .size} states found, ${current_branches.size} yet to explore")
        }
      }
    }

    val current_time: Long = System.nanoTime()
    val generation_time: Long =
      (current_time - generation_start_time) / 1_000_000L
    val total_time: Long = (System.nanoTime() - global_start_time) / 1_000_000L
    logger
      .info(s"RASI generation complete [$i iterations in ${generation_time}ms]")
    logger.info(s"Total runtime: ${total_time}ms")

    // The initial state converts to simply "true", so it would make the RASI trivial
    found_states.filterInPlace(s => s.valuations != initial_state.valuations)
    found_edges.filterInPlace(e =>
      e.from.valuations != initial_state.valuations &&
        e.to.valuations != initial_state.valuations
    )

    logger.debug(
      s"In total, ${found_states.size} states and ${found_edges.size} edges were found"
    )
    logger.debug(
      s"${found_edges.count(e => e.vars.nonEmpty)} edges have variable annotations"
    )
  }

  private def reset(
      node: CFGEntry[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
  ): Long = {
    found_states.clear()
    found_edges.clear()
    current_branches.clear()

    val initial_state = AbstractState(
      get_initial_values(vars),
      HashMap((AbstractProcess[G](Null()(Origin(Seq()))), node)),
      None,
      get_parameter_constraints(parameter_invariant),
    ).with_condition(parameter_invariant.body)

    found_states += initial_state
    current_branches += initial_state

    System.nanoTime()
  }

  private def get_initial_values(
      vars: Set[ConcreteVariable[G]]
  ): Map[ConcreteVariable[G], UncertainValue] = {
    Map.from(vars.map(v => (v, UncertainValue.uncertain_of(v.t))))
  }

  private def get_parameter_constraints(
      parameter_invariant: InstancePredicate[G]
  ): Map[FieldVariable[G], UncertainValue] = {
    val pred = parameter_invariant.body.get
    val parameters: Seq[InstanceField[G]] = pred.transSubnodes.collect {
      case f: Deref[G] => f
    }.map(d => d.ref.decl)
    Map.from(
      parameters.map(f => FieldVariable(f) -> UncertainValue.uncertain_of(f.t))
    )
  }

  private def is_relevant_variable(variable: ConcreteVariable[G]): Boolean = {
    // TODO: This is just a (horrendous) heuristic! This needs to be fixed ASAP!
    // Basically, if the variable is a defined variable from the VeSUV transformation,
    // it should be included when encountered
    val name = variable.to_expression.toInlineString
    val relevant_names: Set[String] = Set(
      ".ProcessState_",
      ".EventState_",
      ".PrimitiveChannelUpdate_",
      ".Written",
      ".NumRead",
      ".Buffer",
    )
    relevant_names.exists(s => name.contains(s))
  }

  private def reduce_redundant_states()
      : (Seq[AbstractState[G]], Seq[(AbstractState[G], AbstractState[G])]) = {
    val state_groups: Map[Expr[G], mutable.ArrayBuffer[AbstractState[G]]] = Map
      .from(found_states.groupBy(s => s.to_expression))
    val edge_groups: Seq[(AbstractState[G], AbstractState[G])] = Seq
      .from(found_edges.map(e =>
        (
          state_groups(e.from.to_expression).head,
          state_groups(e.to.to_expression).head,
        )
      ))
    (
      state_groups.values.toSeq.map(v => v.head),
      edge_groups.distinct.filter(t => t._1 != t._2),
    )
  }
}
