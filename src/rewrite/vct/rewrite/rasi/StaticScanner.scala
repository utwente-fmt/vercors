package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.{CFGEntry, CFGNode}

import scala.collection.mutable

class StaticScanner[G](initial_node: CFGNode[G], state: AbstractState[G]) {
  private val var_change_explored_nodes: mutable.Set[CFGEntry[G]] = mutable.Set
    .empty[CFGEntry[G]]

  def scan_can_change_variables(until: Option[CFGNode[G]]): Boolean = {
    if (until.nonEmpty)
      var_change_explored_nodes.add(until.get)
    scan_for_var_changes_from_node(initial_node)
  }

  private def scan_for_var_changes_from_node(node: CFGNode[G]): Boolean = {
    if (var_change_explored_nodes.contains(node))
      return false
    var_change_explored_nodes.add(node)
    node_changes_vars(node) || node.successors.map(e => e.target).collect {
      case n: CFGNode[G] => n
    }.exists(n => scan_for_var_changes_from_node(n))
  }

  private def node_changes_vars(node: CFGNode[G]): Boolean =
    node.ast_node match {
      case Assign(target, value) => assignment_changes_vars(target, value)
      case Havoc(loc) =>
        state.valuations.keySet.exists(v => v.is_contained_by(loc, state))
      case Assume(assn) => assumption_changes_vars(assn)
      case Inhale(res) => assumption_changes_vars(res)
      case InvokeProcedure(ref, args, _, _, _, _)
          if !ref.decl.pure && ref.decl.body.isEmpty =>
        postcondition_changes_vars(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
        )
      case InvokeConstructor(ref, _, _, args, _, _, _, _)
          if !ref.decl.pure && ref.decl.body.isEmpty =>
        postcondition_changes_vars(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
        )
      case InvokeMethod(_, ref, args, _, _, _, _)
          if !ref.decl.pure && ref.decl.body.isEmpty =>
        postcondition_changes_vars(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
        )
      case _ => false
    }

  private def assignment_changes_vars(
      target: Expr[G],
      value: Expr[G],
  ): Boolean = {
    if (!state.valuations.keySet.exists(v => v.is_contained_by(target, state)))
      return false
    target.t match {
      // If it is a single-value variable, any assignment can change it
      case _: IntType[_] | _: TBool[_] | _: TResource[_] => true
      // If it is a collection, an update might only change other indices than those tracked
      // Therefore: evaluate the assignment explicitly to see if it affects the tracked variables      TODO: Consider arrays
      case _: TSeq[_] =>
        state.to_expression(None) !=
          state.with_updated_collection(target, value).to_expression(None)
    }
  }

  private def assumption_changes_vars(assumption: Expr[G]): Boolean = {
    val potential_successor: RASISuccessor[G] = state
      .with_assumption(assumption)
    if (potential_successor.successors.size != 1)
      true
    else
      state.to_expression(None) !=
        potential_successor.successors.head.to_expression(None)
  }

  private def postcondition_changes_vars(
      postcondition: AccountedPredicate[G],
      params: Map[Variable[G], Expr[G]],
  ): Boolean = {
    val potential_successor: RASISuccessor[G] = state
      .with_postcondition(postcondition, params)
    if (potential_successor.successors.size != 1)
      true
    else
      state.to_expression(None) !=
        potential_successor.successors.head.to_expression(None)
  }
}
