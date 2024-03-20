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
  def atomic_step(starting_state: AbstractState[G]): Set[AbstractState[G]] = {
    var (atomic, states): (Boolean, Set[AbstractState[G]]) = small_step(starting_state.processes(this), starting_state)
    while (atomic) {
      val next: Set[(Boolean, Set[AbstractState[G]])] = states.map(s => small_step_if_atomic(s))
      states = next.flatMap(t => t._2)
      atomic = next.exists(t => t._1)
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
  private def small_step_if_atomic(state: AbstractState[G]): (Boolean, Set[AbstractState[G]]) = {
    if (!state.processes.contains(this) || !is_atomic(state.processes(this))) (false, Set(state))
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
  private def small_step(node: CFGEntry[G], state: AbstractState[G]): (Boolean, Set[AbstractState[G]]) = node match {
    case CFGTerminal() => (false, Set(state.without_process(this)))
    case CFGNode(n, succ) => n match {
      // Assign statements change the state of variables directly (if they appear in the valuation)
      case Assign(target, value) => (true, target.t match {
        case _: IntType[_] | TBool() => viable_edges(succ, state).map(e => take_edge(e, state.with_valuation(target, state.resolve_expression(value))))
        case _: TSeq[_] => viable_edges(succ, state).map(e => take_edge(e, state.with_updated_collection(target, value)))   // TODO: Consider arrays
        case _ => viable_edges(succ, state).map(e => take_edge(e, state))
      })
      case Havoc(loc) => (true, viable_edges(succ, state).map(e => take_edge(e, state.with_valuation(loc, UncertainValue.uncertain_of(loc.t)))))
      // Statements that induce assumptions about the state, such as assume, inhale, or a method's postcondition, might change the state implicitly
      case Assume(assn) => (true, viable_edges(succ, state).flatMap(e => state.with_assumption(assn).map(s => take_edge(e, s))))
      case Inhale(res) => (true, viable_edges(succ, state).flatMap(e => state.with_assumption(res).map(s => take_edge(e, s))))
      // Abstract procedures, constructors and methods are defined by their postconditions      TODO: Handle local variables
      case InvokeProcedure(ref, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => viable_edges(succ, state).map(e => take_edge(e, state))
        case None => viable_edges(succ, state).flatMap(e => state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))).map(s => take_edge(e, s)))
      })
      case InvokeConstructor(ref, _, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => viable_edges(succ, state).map(e => take_edge(e, state))
        case None => viable_edges(succ, state).flatMap(e => state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))).map(s => take_edge(e, s)))
      })
      case InvokeMethod(_, ref, args, _, _, _, _) => (true, ref.decl.body match {
        case Some(_) => viable_edges(succ, state).map(e => take_edge(e, state))
        case None => viable_edges(succ, state).flatMap(e => state.with_postcondition(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args))).map(s => take_edge(e, s)))
      })
      // TODO: Handle local variables
      case Return(result) => (true, viable_edges(succ, state).map(e => take_edge(e, state)))
      // TODO: What do wait and notify do?
      case Wait(obj) => (true, viable_edges(succ, state).map(e => take_edge(e, state)))
      case Notify(obj) => (true, viable_edges(succ, state).map(e => take_edge(e, state)))
      // Lock and Unlock manipulate the global lock and are potentially blocking      TODO: Differentiate between locks!
      case Lock(_) => state.lock match {
        case Some(proc) => if (!proc.equals(this)) (false, Set(state))
                           else throw new IllegalStateException("Trying to lock already acquired lock")
        case None => (true, viable_edges(succ, state).map(e => take_edge(e, state).locked_by(this)))
      }
      case Unlock(_) => state.lock match {    // TODO: Progress was made, but this is still false to allow other processes to execute
        case Some(proc) => if (proc.equals(this)) (false, viable_edges(succ, state).map(e => take_edge(e, state).unlocked()))
                           else throw new IllegalStateException("Trying to unlock lock owned by other process")
        case None => throw new IllegalStateException("Trying to unlock unlocked lock")
      }
      // When forking a new process, make the step of creating it simultaneously to the normal steps
      case Fork(obj) =>
        val edges: (Set[CFGEdge[G]], Set[CFGEdge[G]]) = viable_edges(succ, state).partition(e => e.target match {
          case CFGTerminal() => false
          case CFGNode(t, _) => t.equals(obj.t.asClass.get.cls.decl.declarations.collect{ case r: RunMethod[G] => r }.head.body.get)
        })
        (true, edges._2.map(e => take_edge(e, state.with_process_at(AbstractProcess(obj), edges._1.head.target))))    // TODO: Can only one thread per class instance be launched?
      case Join(obj) =>
        if (state.processes.keys.forall(p => p.obj != obj)) (true, viable_edges(succ, state).map(e => take_edge(e, state)))
        else (false, Set(state))
      // Everything else does not affect the state, so simply go to the next step
      case _ => (true, viable_edges(succ, state).map(e => take_edge(e, state)))
    }
  }

  /**
   * Filters out CFG edges from a given set based on whether they would be viable in the given abstract state.
   *
   * @param edges A set of CFG edges
   * @param state Current abstract state
   * @return A set of those entries of <code>edges</code> whose conditions could evaluate to <code>true</code> in the
   *         given abstract state
   */
  private def viable_edges(edges: mutable.Set[CFGEdge[G]], state: AbstractState[G]): Set[CFGEdge[G]] =
    edges.filter(e => e.condition.isEmpty || state.resolve_boolean_expression(e.condition.get).can_be_true).toSet

  /**
   * Returns the abstract state that results in this process taking a transition in the CFG.
   *
   * @param edge CFG transition edge to take
   * @param state Current abstract state
   * @return Copy of <code>state</code> with this process at the target node of the CFG transition
   */
  private def take_edge(edge: CFGEdge[G], state: AbstractState[G]): AbstractState[G] =
    state.with_process_at(this, edge.target)

  /**
   * Checks whether the considered block is still atomic when adding the next node.
   *
   * @param node CFG node that might be considered next
   * @return <code>false</code> if the node checks the global invariant, <code>true</code> otherwise
   */
  private def is_atomic(node: CFGEntry[G]): Boolean = node match {
    case CFGTerminal() => true
    case CFGNode(n, _) => n match {
      case Unlock(_) => false
      case Assert(res) => Utils.contains_global_invariant(res)
      case _ => true
    }
  }
}
