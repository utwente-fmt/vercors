package vct.rewrite.rasi

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.print.Ctx
import vct.rewrite.cfg.{CFGEntry, CFGGenerator}

import java.nio.file.Path
import scala.collection.mutable

class RASIGenerator[G] extends LazyLogging {
  private val found_states: mutable.ArrayBuffer[AbstractState[G]] = mutable
    .ArrayBuffer()
  private val found_edges: mutable.ArrayBuffer[RASIEdge[G]] = mutable
    .ArrayBuffer()
  private val current_branches: mutable.ArrayBuffer[AbstractState[G]] = mutable
    .ArrayBuffer()
  private var tracked_sequences: Map[InstanceField[G], Set[FieldVariable[G]]] =
    Map.empty[InstanceField[G], Set[FieldVariable[G]]]

  def execute(
      entry_point: Procedure[G],
      vars: Set[FieldVariable[G]],
      split_on: Option[Set[FieldVariable[G]]],
      parameter_invariant: Option[InstancePredicate[G]],
      program: Node[G],
      main_class: ByReferenceClass[G],
      seqs: Set[InstanceField[G]],
  ): Seq[(String, Expr[G])] =
    generate_rasi(
      CFGGenerator().generate(entry_point),
      handle_tracked_sequences(vars, seqs),
      split_on,
      parameter_invariant,
      program,
      main_class,
    )

  def test(
      entry_point: Procedure[G],
      vars: Set[FieldVariable[G]],
      parameter_invariant: Option[InstancePredicate[G]],
      out_path: Path,
      seqs: Set[InstanceField[G]],
  ): Unit =
    print_state_space(
      CFGGenerator().generate(entry_point),
      handle_tracked_sequences(vars, seqs),
      parameter_invariant,
      out_path,
    )

  private def handle_tracked_sequences(
      vars: Set[FieldVariable[G]],
      seqs: Set[InstanceField[G]],
  ): Set[FieldVariable[G]] = {
    tracked_sequences = Map.from(seqs.map(f => f -> Set(FieldSizeVariable(f))))
    vars ++ tracked_sequences.flatMap(t => t._2)
  }

  private def generate_rasi(
      node: CFGEntry[G],
      vars: Set[FieldVariable[G]],
      split_on: Option[Set[FieldVariable[G]]],
      parameter_invariant: Option[InstancePredicate[G]],
      program: Node[G],
      main_class: ByReferenceClass[G],
  ): Seq[(String, Expr[G])] = {
    explore(
      node,
      vars,
      parameter_invariant,
      resolve_global_invariant(main_class),
    )
    val distinct: Int = found_states.distinctBy(s => s.valuations).size
    logger.info(s"$distinct distinct states found")

    if (split_on.isEmpty)
      return Seq((
        "reachable_abstract_states_invariant",
        get_rasi_expression(_ => true, None, program, main_class),
      ))

    val all_processes: Set[AbstractProcess[G]] = found_states.toSet
      .flatMap((s: AbstractState[G]) => s.processes.keySet)

    var res: Seq[(String, Expr[G])] = Seq((
      "interleaving_states",
      get_rasi_expression(s => s.lock.isEmpty, None, program, main_class),
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
              main_class,
            ),
          )
    )

    res
  }

  private def resolve_global_invariant(
      main: ByReferenceClass[G]
  ): Map[FieldVariable[G], UncertainSingleValue] = {
    // TODO: Take into account constraints other than equalities?
    Map.from(
      Utils.split_conjunction(
        main.intrinsicLockInvariant,
        unfold_predicates = true,
      ).collect {
        case Eq(left, right) => (left, right)
        case AmbiguousEq(left, right, _) => (left, right)
      }.map(e => constraint_to_valuation(e._1, e._2)).filter(c => c.nonEmpty)
        .map(c => c.get)
    )
  }

  private def constraint_to_valuation(
      left: Expr[G],
      right: Expr[G],
  ): Option[(FieldVariable[G], UncertainSingleValue)] =
    left match {
      case Deref(_, ref) =>
        right match {
          case IntegerValue(value) =>
            Some((
              FieldSimpleVariable(ref.decl),
              UncertainIntegerValue.single(value.intValue),
            ))
          case _ => None
        }
      case Size(Deref(_, ref)) =>
        right match {
          case IntegerValue(value) =>
            Some((
              FieldSizeVariable(ref.decl),
              UncertainIntegerValue.single(value.intValue),
            ))
          case _ => None
        }
      case IntegerValue(value) =>
        left match {
          case Deref(_, ref) =>
            Some((
              FieldSimpleVariable(ref.decl),
              UncertainIntegerValue.single(value.intValue),
            ))
          case Size(Deref(_, ref)) =>
            Some((
              FieldSizeVariable(ref.decl),
              UncertainIntegerValue.single(value.intValue),
            ))
          case _ => None
        }
      case _ => None
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
      main_class: Class[G],
  ): Expr[G] = {
    val rasi_states = found_states.filter(f)
      .filter(s => proc.isEmpty || s.lock.isEmpty || s.lock.get == proc.get)
      .distinctBy(s => s.valuations)

    if (rasi_states.isEmpty)
      return BooleanValue(value = false)(Utils.origen)

    val objs: Map[FieldVariable[G], Expr[G]] = find_fitting_objects(
      program,
      main_class,
      rasi_states.map(s => s.valuations.keySet).reduce((s1, s2) => s1 ++ s2),
    )

    val always_known: Set[FieldVariable[G]] = rasi_states
      .map(s => s.valuations.keySet).reduce((s1, s2) => s1.intersect(s2))
    val always_the_same: Set[FieldVariable[G]] = always_known
      .filter(v => rasi_states.map(s => s.valuations(v)).distinct.size == 1)
    val constants: Seq[Expr[G]] = always_the_same.toSeq
      .sortWith((v1, v2) => v1.compare(v2)).map(v =>
        rasi_states.head.valuations(v)
          .to_expression(v.to_expression(objs.get(v)))
      )
    val remainder: Expr[G] = rasi_states.map(s =>
      s.without_valuation_of(always_the_same).to_expression(Some(objs))
    ).reduce((e1, e2) => Or(e1, e2)(Utils.origen))

    if (constants.isEmpty)
      remainder
    else
      And(constants.reduce((e1, e2) => And(e1, e2)(Utils.origen)), remainder)(
        Utils.origen
      )
  }

  private def get_var_value_pairs(
      split_on_vars: Set[FieldVariable[G]]
  ): Set[(FieldVariable[G], UncertainSingleValue)] =
    split_on_vars.flatMap(v => found_states.map(s => v -> s.valuations(v)))

  private def get_rasi_name(
      variable: ConcreteVariable[G],
      value: UncertainSingleValue,
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
      vars: Set[FieldVariable[G]],
      parameter_invariant: Option[InstancePredicate[G]],
      out_path: Path,
  ): Unit = {
    explore(node, vars, parameter_invariant, Map.empty)
    val (ns, es) = reduce_redundant_states()
    logger.info(s"${ns.size} distinct states found")
    Utils.print(ns, es, out_path)
  }

  private def explore(
      node: CFGEntry[G],
      vars: Set[FieldVariable[G]],
      parameter_invariant: Option[InstancePredicate[G]],
      invariant_knowledge: Map[FieldVariable[G], UncertainSingleValue],
  ): Unit = {
    logger.info("Starting RASI generation")
    val global_start_time: Long = System.nanoTime()

    var considered_variables: Set[FieldVariable[G]] = vars
      .filter(v => !invariant_knowledge.contains(v))

    var generation_start_time: Long = reset(
      node,
      considered_variables,
      parameter_invariant,
      invariant_knowledge,
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
          invariant_knowledge,
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
      vars: Set[FieldVariable[G]],
      parameter_invariant: Option[InstancePredicate[G]],
      invariant_knowledge: Map[FieldVariable[G], UncertainSingleValue],
  ): Long = {
    found_states.clear()
    found_edges.clear()
    current_branches.clear()

    val initial_state = AbstractState(
      get_initial_values(vars),
      Map.from(Seq((AbstractProcess[G](Null()(Utils.origen)), node))),
      invariant_knowledge,
      Map.empty[LocalVariable[G], UncertainSingleValue],
      Map.empty[Variable[G], Set[FieldVariable[G]]],
      None,
      get_parameter_constraints(parameter_invariant),
      tracked_sequences,
    ).with_condition(parameter_invariant.flatMap(p => p.body))

    found_states += initial_state
    current_branches += initial_state

    System.nanoTime()
  }

  private def get_initial_values(
      vars: Set[FieldVariable[G]]
  ): Map[FieldVariable[G], UncertainSingleValue] = {
    Map.from(vars.map(v =>
      v ->
        (v match {
          case FieldSizeVariable(_) => UncertainIntegerValue.above(0)
          case _ => UncertainSingleValue.uncertain_of(v.t)
        })
    ))
  }

  private def get_parameter_constraints(
      parameter_invariant: Option[InstancePredicate[G]]
  ): Map[FieldSimpleVariable[G], UncertainSingleValue] = {
    if (parameter_invariant.isEmpty)
      return Map.empty[FieldSimpleVariable[G], UncertainSingleValue]
    val pred = parameter_invariant.get.body.get
    val parameters: Seq[InstanceField[G]] = pred.collect { case f: Deref[G] =>
      f
    }.map(d => d.ref.decl)
    Map.from(parameters.map(f =>
      FieldSimpleVariable(f) -> UncertainSingleValue.uncertain_of(f.t)
    ))
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
      main_class: Class[G],
      vars: Set[FieldVariable[G]],
  ): Map[FieldVariable[G], Expr[G]] = {
    var m: Map[FieldVariable[G], Expr[G]] = Map.empty[FieldVariable[G], Expr[G]]

    val classes: Seq[Class[G]] = program.collect { case c: Class[G] => c }

    for (v <- vars) { m += (v -> find_field_object(classes, main_class, v.f)) }

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
