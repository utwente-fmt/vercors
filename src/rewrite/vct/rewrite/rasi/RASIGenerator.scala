package vct.rewrite.rasi

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  AmbiguousThis,
  BooleanValue,
  Class,
  Declaration,
  Deref,
  Expr,
  InstanceField,
  InstancePredicate,
  Node,
  Null,
  Or,
  Procedure,
  TClass,
}
import vct.col.origin.Origin
import vct.col.print.Ctx
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
      entry_point: Procedure[G],
      vars: Set[ConcreteVariable[G]],
      split_on: Option[Set[ConcreteVariable[G]]],
      parameter_invariant: InstancePredicate[G],
      program: Node[G],
  ): Seq[(String, Expr[G])] =
    generate_rasi(
      CFGGenerator().generate(entry_point),
      vars,
      split_on,
      parameter_invariant,
      program,
    )

  def test(
      entry_point: Procedure[G],
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
      split_on: Option[Set[ConcreteVariable[G]]],
      parameter_invariant: InstancePredicate[G],
      program: Node[G],
  ): Seq[(String, Expr[G])] = {
    explore(node, vars, parameter_invariant)
    val distinct: Int = found_states.distinctBy(s => s.valuations).size
    logger.info(s"$distinct distinct states found")

    if (split_on.isEmpty)
      return Seq((
        "reachable_abstract_states_invariant",
        get_rasi_expression(_ => true, None, program),
      ))

    val all_processes: Set[AbstractProcess[G]] = found_states.toSet
      .flatMap((s: AbstractState[G]) => s.processes.keySet)

    var res: Seq[(String, Expr[G])] = Seq((
      "interleaving_states",
      get_rasi_expression(s => s.lock.isEmpty, None, program),
    ))

    get_var_value_pairs(split_on.get).foreach(t =>
      res =
        res :+
          (
            get_rasi_name(t._1, t._2),
            get_rasi_expression(
              s => s.valuations(t._1) == t._2,
              Some(get_associated_process(t._1, all_processes)),
              program,
            ),
          )
    )

    res
  }

  private def get_associated_process(
      of: ConcreteVariable[G],
      from: Set[AbstractProcess[G]],
  ): AbstractProcess[G] = {
    from.filter(p => p.obj.t.isInstanceOf[TClass[G]]).collectFirst {
      case p @ AbstractProcess(obj)
          if obj.t.asInstanceOf[TClass[G]].cls.decl.decls
            .contains(of.get_declaration) =>
        p
    }.get
  }

  private def get_rasi_expression(
      f: AbstractState[G] => Boolean,
      proc: Option[AbstractProcess[G]],
      program: Node[G],
  ): Expr[G] = {
    val rasi_states = found_states.filter(f)
      .filter(s => proc.isEmpty || s.lock.isEmpty || s.lock.get == proc.get)
      .distinctBy(s => s.valuations)

    if (rasi_states.isEmpty)
      return BooleanValue(value = false)(program.o)

    val objs: Map[ConcreteVariable[G], Expr[G]] = find_fitting_objects(
      program,
      rasi_states.head.valuations.keySet,
    )
    rasi_states.map(s => s.to_expression(Some(objs)))
      .reduce((e1, e2) => Or(e1, e2)(e1.o))
  }

  private def get_var_value_pairs(
      split_on_vars: Set[ConcreteVariable[G]]
  ): Set[(ConcreteVariable[G], UncertainValue)] =
    split_on_vars.flatMap(v => found_states.map(s => v -> s.valuations(v)))

  private def get_rasi_name(
      variable: ConcreteVariable[G],
      value: UncertainValue,
  ): String = {
    // Compute variable name
    val name_map: Map[Declaration[_], String] = Map.from(Seq(
      variable.get_declaration ->
        variable.get_declaration.o.getPreferredName.get.snake
    ))
    implicit val context: Ctx = Ctx(syntax = Ctx.PVL, names = name_map)
    val var_name: String = variable.to_expression(None).toStringWithContext
      .replace("]", "").replace("[", "").replace("this.", "")

    // Compute value string
    val value_string: String =
      value match {
        case i: UncertainIntegerValue =>
          i.try_to_resolve()
            .getOrElse(throw new IllegalStateException("Value must be defined"))
            .toString
        case b: UncertainBooleanValue =>
          b.try_to_resolve()
            .getOrElse(throw new IllegalStateException("Value must be defined"))
            .toString
      }

    "rasi_" + var_name + "_" + value_string
  }

  private def print_state_space(
      node: CFGEntry[G],
      vars: Set[ConcreteVariable[G]],
      parameter_invariant: InstancePredicate[G],
      out_path: Path,
  ): Unit = {
    explore(node, vars, parameter_invariant)
    val (ns, es) = reduce_redundant_states()
    logger.info(s"${ns.size} distinct states found")
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

      if (successor.distinguish_by.nonEmpty) {
        val time: Long =
          (System.nanoTime() - generation_start_time) / 1_000_000L
        logger.info(
          s"Found relevant new variables; abort generation [$i iterations in ${time}ms]"
        )
        val found_vars: Seq[String] = successor.distinguish_by.toSeq
          .sortWith((v1, v2) => v1.compare(v2))
          .map(v => v.to_expression(None).toInlineString)
        logger.debug(s"Variables found: $found_vars")

        considered_variables ++= successor.distinguish_by

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
            found_states += s
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
    val parameters: Seq[InstanceField[G]] = pred.collect { case f: Deref[G] =>
      f
    }.map(d => d.ref.decl)
    Map.from(
      parameters.map(f => FieldVariable(f) -> UncertainValue.uncertain_of(f.t))
    )
  }

  private def reduce_redundant_states()
      : (Seq[AbstractState[G]], Seq[(AbstractState[G], AbstractState[G])]) = {
    val state_groups: Map[Expr[G], mutable.ArrayBuffer[AbstractState[G]]] = Map
      .from(found_states.groupBy(s => s.to_expression(None)))
    val edge_groups: Seq[(AbstractState[G], AbstractState[G])] = Seq
      .from(found_edges.map(e =>
        (
          state_groups(e.from.to_expression(None)).head,
          state_groups(e.to.to_expression(None)).head,
        )
      ))
    (
      state_groups.values.toSeq.map(v => v.head),
      edge_groups.distinct.filter(t => t._1 != t._2),
    )
  }

  private def find_fitting_objects(
      program: Node[G],
      vars: Set[ConcreteVariable[G]],
  ): Map[ConcreteVariable[G], Expr[G]] = {
    var m: Map[ConcreteVariable[G], Expr[G]] = Map
      .empty[ConcreteVariable[G], Expr[G]]

    val classes: Seq[Class[G]] = program.collect[Class[G]] { case c: Class[G] =>
      c
    }
    // TODO: Find differently, e.g. with lock invariant?
    val main_class: Class[G] =
      classes.find(c => c.o.getPreferredName.get.camel.startsWith("main")).get

    for (v <- vars) {
      v match {
        case IndexedVariable(f, _) =>
          m += (v -> find_field_object(classes, main_class, f))
        case FieldVariable(f) =>
          m += (v -> find_field_object(classes, main_class, f))
        case SizeVariable(f) =>
          m += (v -> find_field_object(classes, main_class, f))
        case LocalVariable(f) => m += (v -> AmbiguousThis()(f.o))
      }
    }

    m
  }

  private def find_field_object(
      classes: Seq[Class[G]],
      main: Class[G],
      field: InstanceField[G],
  ): Expr[G] = {
    val type_class: Class[G] = classes.find(c => c.decls.contains(field)).get
    if (type_class == main)
      return AmbiguousThis()(field.o)

    val obj: InstanceField[G] =
      main.decls.collectFirst {
        case f: InstanceField[G]
            if f.t.isInstanceOf[TClass[G]] &&
              f.t.asInstanceOf[TClass[G]].cls.decl == type_class =>
          f
      }.get
    Deref[G](AmbiguousThis()(field.o), obj.ref)(field.o)(field.o)
  }
}
