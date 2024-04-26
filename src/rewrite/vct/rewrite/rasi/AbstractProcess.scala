package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.{CFGEdge, CFGEntry, CFGNode, CFGTerminal}

import scala.collection.mutable

case class AbstractProcess[G](obj: Expr[G]) {

  /**
   * Simulates an atomic execution step for this process on the given abstract state.
   *
   * @param starting_state Abstract starting state
   * @return A set of all successor states
   */
  def atomic_step(starting_state: AbstractState[G]): RASISuccessor[G] = {
    var (atomic, states): (Boolean, RASISuccessor[G]) = small_step(starting_state.processes(this), starting_state)
    while (atomic) {
      val next: Set[(Boolean, RASISuccessor[G])] = states.successors.map(s => small_step_if_atomic(s))
      atomic = next.exists(t => t._1)
      val next_states: Set[RASISuccessor[G]] = next.map(s => s._2)
      val variables: Set[ConcreteVariable[G]] = states.deciding_variables ++ next_states.flatMap(s => s.deciding_variables)
      val successors: Set[AbstractState[G]] = next_states.flatMap(s => s.successors)
      states = RASISuccessor(variables, successors)
    }
    states
  }

  /**
   * Tests if another small step can be executed without breaking the current atomic block, and if so, executes the next
   * small step.
   *
   * @param state Current abstract state
   * @return A tuple of a boolean indicating whether atomic progress could be made and a set of all successor states
   */
  private def small_step_if_atomic(state: AbstractState[G]): (Boolean, RASISuccessor[G]) = {
    if (!state.processes.contains(this) || !is_atomic(state.processes(this))) (false, RASISuccessor(Set(), Set(state)))
    else small_step(state.processes(this), state)
  }

  /**
   * Makes a small step in the symbolic execution. Executes the effect of the given CFG node on the given abstract state
   * and returns a tuple of a boolean indicating whether progress was made and a set of all possible successor states.
   *
   * @param node CFG node to simulate
   * @param state Current abstract state
   * @return A tuple of a boolean indicating whether progress was made and a set of all successor states.
   */
  private def small_step(node: CFGEntry[G], state: AbstractState[G]): (Boolean, RASISuccessor[G]) = node match {
    case CFGTerminal() => (false, RASISuccessor(Set(), Set(state.without_process(this))))
    case CFGNode(n, succ) => n match {
      // Assign statements change the state of variables directly (if they appear in the valuation)
      case Assign(target, value) => (true, target.t match {
        case _: IntType[_] | TBool() => take_viable_edges(succ, state, RASISuccessor(Set(), Set(state.with_valuation(target, state.resolve_expression(value)))))
        case _: TSeq[_] => take_viable_edges(succ, state, RASISuccessor(Set(), Set(state.with_updated_collection(target, value))))   // TODO: Consider arrays
        case _ => take_viable_edges_from_state(succ, state)
      })
      case Havoc(loc) => (true, take_viable_edges(succ, state, RASISuccessor(Set(), Set(state.with_valuation(loc, UncertainValue.uncertain_of(loc.t))))))
      // Statements that induce assumptions about the state, such as assume, inhale, or a method's postcondition, might change the state implicitly
      case Assume(assn) => (true, take_viable_edges(succ, state, state.with_assumption(assn)))
      case Inhale(res) => (true, take_viable_edges(succ, state, state.with_assumption(res)))
      // Abstract procedures, constructors and methods are defined by their postconditions      TODO: Handle local variables
      case InvokeProcedure(ref, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => take_viable_edges_from_state(succ, state)
        case None => take_viable_edges(succ, state, state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))))
      })
      case InvokeConstructor(ref, _, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => take_viable_edges_from_state(succ, state)
        case None => take_viable_edges(succ, state, state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))))
      })
      case InvokeMethod(_, ref, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => take_viable_edges_from_state(succ, state)
        case None => take_viable_edges(succ, state, state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))))
      })
      // TODO: Handle local variables
      case Return(result) => (true, take_viable_edges_from_state(succ, state))
      // TODO: What do wait and notify do?
      case Wait(obj) => (true, take_viable_edges_from_state(succ, state))
      case Notify(obj) => (true, take_viable_edges_from_state(succ, state))
      // Lock and Unlock manipulate the global lock and are potentially blocking      TODO: Differentiate between locks!
      case Lock(_) => state.lock match {
        case Some(proc) => if (!proc.equals(this)) (false, RASISuccessor(Set(), Set(state)))
                           else throw new IllegalStateException("Trying to lock already acquired lock")
        case None => (true, take_viable_edges(succ, state, RASISuccessor(Set(), Set(state.locked_by(this)))))
      }
      case Unlock(_) => state.lock match {    // Progress was made, but the atomic flag is still false to allow other processes to execute
        case Some(proc) => if (proc.equals(this)) (false, take_viable_edges(succ, state, RASISuccessor(Set(), Set(state.unlocked()))))
                           else throw new IllegalStateException("Trying to unlock lock owned by other process")
        case None => throw new IllegalStateException("Trying to unlock unlocked lock")
      }
      // When forking a new process, make the step of creating it simultaneously to the normal steps
      case Fork(obj) =>
        val edges: (mutable.Set[CFGEdge[G]], mutable.Set[CFGEdge[G]]) = succ.partition(e => e.target match {
          case CFGTerminal() => false
          case CFGNode(t, _) => t.equals(obj.t.asClass.get.cls.decl.declarations.collect{ case r: RunMethod[G] => r }.head.body.get)
        })                // TODO: Can only one thread per class instance be launched?
        (true, take_viable_edges(edges._2, state, RASISuccessor(Set(), Set(state.with_process_at(AbstractProcess(obj), edges._1.head.target)))))
      case Join(obj) =>
        if (state.processes.keys.forall(p => p.obj != obj)) (true, take_viable_edges_from_state(succ, state))
        else (false, RASISuccessor(Set(), Set(state)))
      // Everything else does not affect the state, so simply go to the next step
      case _ => (true, take_viable_edges_from_state(succ, state))
    }
  }

  private def take_viable_edges_from_state(edges: mutable.Set[CFGEdge[G]], state: AbstractState[G]): RASISuccessor[G] =
    take_viable_edges(edges, state, RASISuccessor(Set(), Set(state)))
  /**
   * Returns a successor object representing all states reachable in one state from the given starting state with the
   * given control flow graph edges.
   *
   * @param edges Edges that are possible to take (might not be enabled)
   * @param starting_state Starting state for the step
   * @param new_states Set of states possibly updated by the explored statement on which the steps are taken. If the
   *                   explored statement does not change the state, then <code>new_state == {starting_state}</code>
   * @return A successor object that contains all possible successor states
   */
  private def take_viable_edges(edges: mutable.Set[CFGEdge[G]], starting_state: AbstractState[G], new_states: RASISuccessor[G]): RASISuccessor[G] = {
    val enabled_edges: (Set[CFGEdge[G]], Set[ConcreteVariable[G]]) = viable_edges(edges, starting_state)
    RASISuccessor(new_states.deciding_variables ++ enabled_edges._2, enabled_edges._1.flatMap(e => new_states.successors.map(s => take_edge(e, s))))
  }

  /**
   * Filters out CFG edges from a given set based on whether they would be viable in the given abstract state.
   *
   * @param edges A set of CFG edges
   * @param state Current abstract state
   * @return A set of those entries of <code>edges</code> whose conditions could evaluate to <code>true</code> in the
   *         given abstract state
   */
  private def viable_edges(edges: mutable.Set[CFGEdge[G]], state: AbstractState[G]): (Set[CFGEdge[G]], Set[ConcreteVariable[G]]) = {
    val viable: Set[CFGEdge[G]] = edges.filter(e => e.condition.isEmpty || state.resolve_boolean_expression(e.condition.get).can_be_true).toSet
    val variables: Set[ConcreteVariable[G]] = new VariableSelector(state).deciding_variables(viable.map(e => e.condition))
    (viable, variables)
  }

  /**
   * Returns the abstract state that results in this process taking a transition in the CFG.
   *
   * @param edge CFG transition edge to take
   * @param state Current abstract state
   * @return Copy of <code>state</code> with this process at the target node of the CFG transition
   */
  private def take_edge(edge: CFGEdge[G], state: AbstractState[G]): AbstractState[G] =
    state.with_process_at(this, edge.target).with_condition(edge.condition)

  /**
   * Checks whether the considered block is still atomic when adding the next node.
   *
   * @param node CFG node that might be considered next
   * @return <code>false</code> if the node checks the global invariant, <code>true</code> otherwise
   */
  private def is_atomic(node: CFGEntry[G]): Boolean = node match {
    case CFGTerminal() => true
    case CFGNode(n, _) => n match {
      case Assert(res) => !Utils.contains_global_invariant(res)
      case _ => true
    }
  }
}
