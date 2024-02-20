package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.{CFGEdge, CFGEntry, CFGNode, CFGTerminal}

case class AbstractProcess[G](name: String) {
  def get_next(node: CFGEntry[G], state: AbstractState[G]): Set[AbstractState[G]] = node match {
    case CFGTerminal() => Set()
    case CFGNode(n, succ) => succ.filter(e => e.condition.isEmpty || state.resolve_boolean_expression(e.condition.get).can_be_true)
                                 .map(e => process_cfg_edge(e, n, state)).toSet
  }

  private def process_cfg_edge(edge: CFGEdge[G], ast_node: Node[G], state: AbstractState[G]): AbstractState[G] = ast_node match {
    // TODO: Implement!
    case Assign(target, value) => state.with_valuation(target, state.resolve_integer_expression(value).try_to_resolve().get)
    case Exhale(res) => ???
    case Assert(res) => ???
    case Refute(assn) => ???
    case Inhale(res) => ???
    case Assume(assn) => ???
    case Instantiate(_, out) => ???
    case Wait(_) => ???
    case Notify(_) => ???
    case Fork(obj) => ???   // TODO: This needs to be decided in the outer method...
    case Join(_) => ???
    case Lock(_) => ???
    case Unlock(_) => ???
    case Commit(_) => ???
    case Fold(res) => ???
    case Unfold(res) => ???
    case WandApply(res) => ???
    case Havoc(_) => ???
    case FramedProof(_, _, _) => ???
    case Extract(_) => ???
    case Eval(_) => ???
    case Return(result) => ???
    case Throw(obj) => ???
    case Break(label) => ???
    case Continue(label) => ???
    case InvokeProcedure(_, _, _, _, _, _) => ???
    case InvokeConstructor(_, _, _, _, _, _, _) => ???
    case InvokeMethod(_, _, _, _, _, _, _) => ???
    case Block(_) => ???
    case Scope(_, _) => ???
    case Branch(_) => ???
    case IndetBranch(branches) => ???
    case Switch(expr, body) => ???
    case Loop(_, _, _, _, _) => ???
    case RangedFor(_, _, _) => ???
    case TryCatchFinally(_, _, _) => ???
    case Synchronized(_, _) => ???
    case ParInvariant(_, _, _) => ???
    case ParAtomic(_, _) => ???
    case ParBarrier(_, _, _, _, _) => ???
    case ParStatement(_) => ???
    case VecBlock(_, _, _, _) => ???
    case WandPackage(_, _) => ???
    case ModelDo(_, _, _, _, _) => ???
    case CDeclarationStatement(_) => ???
    case CGoto(label) => ???
    case CPPDeclarationStatement(_) => ???
    case CPPLifetimeScope(_) => ???
    case JavaLocalDeclarationStatement(_) => ???
    case SilverNewRef(_, _) => ???
    case SilverFieldAssign(_, _, value) => ???
    case SilverLocalAssign(_, value) => ???
    case PVLCommunicate(_, _) => ???
    case PVLSeqAssign(_, _, value) => ???
    case Communicate(_, _) => ???
    case SeqAssign(_, _, value) => ???
    case UnresolvedSeqBranch(branches) => ???
    case UnresolvedSeqLoop(_, _, _) => ???
    case _ => state.with_process_at(this, edge.target)
  }
}
