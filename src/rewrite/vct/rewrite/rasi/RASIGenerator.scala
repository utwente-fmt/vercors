package vct.rewrite.rasi

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{Expr, InstanceField, InstanceMethod, Null, Or}
import vct.col.origin.Origin
import vct.rewrite.cfg.{CFGEntry, CFGGenerator}

import java.nio.file.Path
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class RASIGenerator[G]() extends LazyLogging {
  private val found_states: mutable.ArrayBuffer[AbstractState[G]] = mutable.ArrayBuffer()
  private val found_edges: mutable.ArrayBuffer[(AbstractState[G], AbstractState[G])] = mutable.ArrayBuffer()
  private val current_branches: mutable.ArrayBuffer[AbstractState[G]] = mutable.ArrayBuffer()

  def execute(entry_point: InstanceMethod[G], vars: Set[ConcreteVariable[G]]): Expr[G] =
    generate_rasi(CFGGenerator().generate(entry_point), vars)

  def test(entry_point: InstanceMethod[G], vars: Set[ConcreteVariable[G]], out_path: Path): Unit =
    print_state_space(CFGGenerator().generate(entry_point), vars, out_path)

  private def generate_rasi(node: CFGEntry[G], vars: Set[ConcreteVariable[G]]): Expr[G] = {
    explore(node, vars)
    val distinct_states = found_states.distinctBy(s => s.valuations)
    logger.debug(s"${distinct_states.size} distinct states found")
    distinct_states.map(s => s.to_expression).reduce((e1, e2) => Or(e1, e2)(e1.o))
  }

  private def print_state_space(node: CFGEntry[G], vars: Set[ConcreteVariable[G]], out_path: Path): Unit = {
    explore(node, vars)
    val (ns, es) = reduce_redundant_states()
    logger.debug(s"${ns.size} distinct states found")
    Utils.print(ns, es, out_path)
  }

  private def explore(node: CFGEntry[G], vars: Set[ConcreteVariable[G]]): Unit = {
    logger.info("Starting RASI generation")
    val initial_state = AbstractState(get_initial_values(vars),
                                      HashMap((AbstractProcess[G](Null()(Origin(Seq()))), node)),
                                      None,
                                      Map.empty[InstanceField[G], UncertainIntegerValue])
    found_states += initial_state
    current_branches += initial_state

    var i = 0

    while (current_branches.nonEmpty) {
      val curr: AbstractState[G] = current_branches.head
      current_branches -= curr

      val successors: Set[AbstractState[G]] = curr.successors()
      found_edges.addAll(successors.map(s => (curr, s)))
      successors.foreach(s => if (!found_states.contains(s)) {found_states += s; current_branches += s})
      i = i + 1
      if (i % 100 == 0) logger.debug(s"Iteration $i: ${found_states.size} states found, ${current_branches.size} yet to explore")
    }

    logger.info("RASI generation complete")

    // The initial state converts to simply "true", so it would make the RASI trivial
    found_states.filterInPlace(s => s.valuations != initial_state.valuations)
    found_edges.filterInPlace(t => t._1.valuations != initial_state.valuations && t._2.valuations != initial_state.valuations)
  }

  private def get_initial_values(vars: Set[ConcreteVariable[G]]): Map[ConcreteVariable[G], UncertainValue] = {
    Map.from(vars.map(v => (v, UncertainValue.uncertain_of(v.t))))
  }

  private def reduce_redundant_states(): (Seq[AbstractState[G]], Seq[(AbstractState[G], AbstractState[G])]) = {
    val state_groups: Map[Map[ConcreteVariable[G], UncertainValue], ArrayBuffer[AbstractState[G]]] = Map.from(found_states.groupBy(s => s.valuations))
    val edge_groups: Seq[(AbstractState[G], AbstractState[G])] = Seq.from(found_edges.map(t => (state_groups(t._1.valuations).head, state_groups(t._2.valuations).head)).distinct)
    (state_groups.values.toSeq.map(v => v.head), edge_groups.filter(t => t._1 != t._2))
  }
}
