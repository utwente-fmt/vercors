package vct.rewrite.cfg

import vct.col.ast._

object CFGGenerator {

  def generate[G](entry: InstanceMethod[G]): CFGNode[G] = {
    convert(entry.body.get, GlobalIndex[G])
  }

  def convert[G](node: Statement[G], context: GlobalIndex[G]): CFGNode[G] =
    CFGNode(node, find_successors(node, context))

  def find_successors[G](node: Statement[G], context: GlobalIndex[G]): Set[CFGNode[G]] = node match {
    case PVLBranch(branches) =>
      branches.map(b => convert(b._2, context.enter_scope(node))).toSet
    case PVLLoop(init, cond, update, _, body) => {
      val cond_node: Eval[G] = Eval(cond)(cond.o)

    }
    // NonExecutableStatement
    case LocalDecl(local) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case SpecIgnoreStart() =>
      CFGNode(node, successors)
    case SpecIgnoreEnd() =>
      CFGNode(node, successors)
    // NormallyCompletingStatement
    case Assign(target, value) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Send(decl, delta, res) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Recv(ref) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case DefaultCase() => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Case(pattern) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Label(decl, stat) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Goto(lbl) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Exhale(res) => // TODO: Can expressions in specifications be ignored?
      CFGNode(node, successors)
    case Assert(res) =>
      CFGNode(node, successors)
    case Refute(assn) =>
      CFGNode(node, successors)
    case Inhale(res) =>
      CFGNode(node, successors)
    case Assume(assn) =>          // <--
      CFGNode(node, successors)
    case Instantiate(cls, out) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Wait(obj) =>
      CFGNode(node, successors)
    case Notify(obj) =>
      CFGNode(node, successors)
    case Fork(obj) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Join(obj) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Lock(obj) =>
      CFGNode(node, successors)
    case Unlock(obj) =>
      CFGNode(node, successors)
    case Commit(obj) =>
      CFGNode(node, successors)
    case Fold(res) => // TODO: Can expressions in specifications be ignored?
      CFGNode(node, successors)
    case Unfold(res) =>
      CFGNode(node, successors)
    case WandApply(res) =>        // <--
      CFGNode(node, successors)
    case Havoc(loc) =>
      CFGNode(node, successors)
    case FramedProof(pre, body, post) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Extract(contractedStatement) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    // ExceptionalStatement
    case Eval(expr) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    // InvocationStatement
    case InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case InvokeConstructor(ref, out, args, outArgs, typeArgs, givenMap, yields) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields) => {
      CFGNode[G](node, Set[CFGNode[G]])
    } // TODO
    case Return(result) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Throw(obj) =>
      CFGNode(node, successors)
    case Break(label) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Continue(label) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    // CompositeStatement
    case Block(statements) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Scope(locals, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Branch(branches) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case IndetBranch(branches) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Switch(expr, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Loop(init, cond, update, contract, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case RangedFor(iter, contract, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case TryCatchFinally(body, after, catches) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Synchronized(obj, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case ParInvariant(decl, inv, content) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case ParAtomic(inv, content) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case ParBarrier(block, invs, requires, ensures, content) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case ParStatement(impl) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case VecBlock(iters, requires, ensures, content) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case WandPackage(res, proof) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case ModelDo(model, perm, after, action, impl) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    // CStatement
    case CDeclarationStatement(decl) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case CGoto(label) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    // CPPStatement
    case CPPDeclarationStatement(decl) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case CPPLifetimeScope(body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case JavaLocalDeclarationStatement(decl) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    // SilverStatement
    case SilverNewRef(v, fields) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case SilverFieldAssign(obj, field, value) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case SilverLocalAssign(v, value) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    // OTHER
    case PVLCommunicate(sender, receiver) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case PVLSeqAssign(receiver, field, value) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case Communicate(receiver, sender) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case SeqAssign(receiver, field, value) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case UnresolvedSeqBranch(branches) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case UnresolvedSeqLoop(cond, contract, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case SeqBranch(guards, yes, no) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case SeqLoop(guards, contract, body) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case VeyMontAssignExpression(endpoint, assign) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
    case CommunicateX(receiver, sender, chanType, assign) =>
      CFGNode[G](node, Set[CFGNode[G]]) // TODO
  }
}
