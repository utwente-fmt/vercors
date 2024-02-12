package vct.rewrite.cfg

import vct.col.ast._

import scala.collection.mutable

case class CFGGenerator[G]() {
  private val found_labels: mutable.Map[LabelDecl[G], CFGNode[G]] = mutable.HashMap[LabelDecl[G], CFGNode[G]]()
  //private val searched_labels: mutable.Map[LabelDecl[G], mutable.Set[CFGNode[G]]] = mutable.HashMap[LabelDecl[G], mutable.Set[CFGNode[G]]]()
  private val searched_labels: mutable.Map[LabelDecl[G], mutable.Set[CFGNode[G]]] = mutable.HashMap[LabelDecl[G], mutable.Set[CFGNode[G]]]()

  def generate(entry: InstanceMethod[G]): CFGNode[G] = {
    convert(entry.body.get, GlobalIndex[G])
  }

  def convert(node: Statement[G], context: GlobalIndex[G]): CFGNode[G] = {
    // Create new node with its successors (if possible)
    val cfg_node: CFGNode[G] = CFGNode(node, find_successors(node, context))
    // Handle labels and goto statements
    node match {
      case label: Label[G] => {
        // For labels, add them to the label map and add them to any goto statements going to that label
        found_labels.addOne((label.decl, cfg_node))
        searched_labels.getOrElse(label.decl, mutable.Set()).map(g => g.successors.addOne(cfg_node))
      }
      case goto: Goto[G] => {
        // For goto statements, if the label could not be resolved below, add the statement to the waiting list for the right label
        if (cfg_node.successors.isEmpty) {
          if (searched_labels.contains(goto.lbl.decl)) searched_labels(goto.lbl.decl).addOne(cfg_node)
          else searched_labels.addOne((goto.lbl.decl, mutable.Set(cfg_node)))
        }
      }
    }
    cfg_node
  }

  def find_successors(node: Statement[G], context: GlobalIndex[G]): mutable.Set[CFGNode[G]] = node match {
    case PVLBranch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1._2, context.enter_scope(node, b._2))))   // TODO: Implement support for statements in expressions
    case PVLLoop(init, cond, update, _, body) => {
      val cond_node: Eval[G] = Eval(cond)(cond.o)
      // TODO: Implement!
      ???
    }
    // NonExecutableStatement
    case LocalDecl(_) => sequential_successor(context)
    case SpecIgnoreStart() => sequential_successor(context)    // What is this?
    case SpecIgnoreEnd() => sequential_successor(context)
    // NormallyCompletingStatement
    case Assign(target, value) => sequential_successor(context)  // TODO: Implement support for statements in expressions
    case Send(_, _, res) => sequential_successor(context)   // TODO: Implement support for statements in expressions
    case Recv(_) => sequential_successor(context)
    case DefaultCase() => ???   // TODO: Ask Pieter about switch statements
    case Case(pattern) => ???
    case Label(_, stat) =>
      mutable.Set(convert(stat, context.enter_scope(node)))
    case Goto(lbl) => {
      val found_node: Option[CFGNode[G]] = found_labels.get(lbl.decl)
      found_node match {
        case Some(node) => mutable.Set(node)
        case None => mutable.Set()
      }
    }
    case Exhale(res) => sequential_successor(context)  // TODO: Can expressions in specifications be ignored?
    case Assert(res) => sequential_successor(context)
    case Refute(assn) => sequential_successor(context)
    case Inhale(res) => sequential_successor(context)
    case Assume(assn) => sequential_successor(context)  // <--
    case Instantiate(cls, out) => ??? // TODO
    case Wait(_) => sequential_successor(context)
    case Notify(_) => sequential_successor(context)
    case Fork(obj) => ???   // TODO: Find run method from expression
    case Join(_) => sequential_successor(context)
    case Lock(_) => sequential_successor(context)
    case Unlock(_) => sequential_successor(context)
    case Commit(_) => sequential_successor(context)
    case Fold(res) => sequential_successor(context)   // TODO: Can expressions in specifications be ignored?
    case Unfold(res) => sequential_successor(context)
    case WandApply(res) => sequential_successor(context)   // <--
    case Havoc(_) => sequential_successor(context)
    case FramedProof(pre, body, post) =>
      mutable.Set(convert(body, context.enter_scope(node)))     // TODO: Can expressions in specifications be ignored?
    case Extract(contractedStatement) =>
      mutable.Set(convert(contractedStatement, context.enter_scope(node)))
    // ExceptionalStatement
    case Eval(expr) => ??? // TODO
    // InvocationStatement
    case InvokeProcedure(ref, args, _, _, givenMap, _) => {
      if (args.nonEmpty) mutable.Set(convert(Eval(args.head)(args.head.o), context.enter_scope(node)))
      else if (ref.decl.body.nonEmpty) mutable.Set(convert(ref.decl.body.get, context.enter_scope(node)))
      else sequential_successor(context)
    }
    case InvokeConstructor(ref, out, args, outArgs, typeArgs, givenMap, yields) => {
      if (args.nonEmpty) mutable.Set(convert(Eval(args.head)(args.head.o), context.enter_scope(node)))
      else if (ref.decl.body.nonEmpty) mutable.Set(convert(ref.decl.body.get, context.enter_scope(node)))
      else sequential_successor(context)
    }
    case InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields) => {
      if (args.nonEmpty) mutable.Set(convert(Eval(args.head)(args.head.o), context.enter_scope(node)))
      else if (ref.decl.body.nonEmpty) mutable.Set(convert(ref.decl.body.get, context.enter_scope(node)))
      else sequential_successor(context)
    }
    case Return(result) => mutable.Set(return_successor(context))
    case Throw(obj) => mutable.Set(exception_successor(obj, context))
    case Break(label) => label match {
      case Some(ref) => ???  // TODO: Handle break label!
      case None => mutable.Set(break_successor(context))
    }
    case Continue(label) => label match {
      case Some(ref) => ???  // TODO: Handle continue label!
      case None => mutable.Set(continue_successor(context))
    }
    // CompositeStatement
    case Block(statements) =>
      mutable.Set(convert(statements.head, context.enter_scope(node)))
    case Scope(_, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case Branch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1._2, context.enter_scope(node, b._2))))   // TODO: Handle statements in condition expressions!
    case IndetBranch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1, context.enter_scope(node, b._2))))
    case Switch(expr, body) => ??? // TODO: Ask Pieter about switch statements
    case Loop(init, cond, update, _, body) => {
      ???   // TODO
    }
    case RangedFor(_, _, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case TryCatchFinally(body, _, _) =>
      mutable.Set(convert(body, context.enter_scope(node)))   // TODO: Is it safe to ignore the "after" part here?
    case Synchronized(_, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case ParInvariant(_, _, content) =>
      mutable.Set(convert(content, context.enter_scope(node)))
    case ParAtomic(_, content) =>
      mutable.Set(convert(content, context.enter_scope(node)))
    case ParBarrier(_, _, _, _, content) =>
      mutable.Set(convert(content, context.enter_scope(node)))
    case ParStatement(_) => sequential_successor(context)
    case VecBlock(_, _, _, content) =>
      mutable.Set(convert(content, context.enter_scope(node)))
    case WandPackage(res, proof) =>
      ???   // TODO
    case ModelDo(model, perm, after, action, impl) =>
      ???   // TODO
    // CStatement
    case CDeclarationStatement(_) => sequential_successor(context)
    case CGoto(label) => ???
    // CPPStatement
    case CPPDeclarationStatement(_) => sequential_successor(context)
    case CPPLifetimeScope(body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case JavaLocalDeclarationStatement(_) => sequential_successor(context)
    // SilverStatement
    case SilverNewRef(_, _) => sequential_successor(context)
    case SilverFieldAssign(_, _, value) => sequential_successor(context)   // TODO: Statements in expressions
    case SilverLocalAssign(_, value) => sequential_successor(context)   // TODO: Statements in expressions
    // OTHER
    case PVLCommunicate(_, _) => sequential_successor(context)
    case PVLSeqAssign(_, _, value) => sequential_successor(context)   // TODO: Statements in expressions
    case Communicate(_, _) => sequential_successor(context)
    case SeqAssign(_, _, value) => sequential_successor(context)      // TODO: Statements in expressions
    case UnresolvedSeqBranch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1._2, context.enter_scope(node, b._2))))
    case UnresolvedSeqLoop(cond, _, body) => {
      ???   // TODO: Implement
    }
    case SeqBranch(_, yes, no) => no match {
      case Some(stmt) => mutable.Set(convert(yes, context.enter_scope(node, 0)), convert(stmt, context.enter_scope(node, 1)))
      case None => mutable.Set(convert(yes, context.enter_scope(node)))
    }
    case SeqLoop(_, _, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case VeyMontAssignExpression(_, assign) =>
      mutable.Set(convert(assign, context.enter_scope(node)))
    case CommunicateX(_, _, _, assign) =>
      mutable.Set(convert(assign, context.enter_scope(node)))
  }

  def sequential_successor(index: GlobalIndex[G]): mutable.Set[CFGNode[G]] = {
    val next_index: GlobalIndex[G] = index.make_step()
    val next_statement: Statement[G] = next_index.resolve()

  }

  def return_successor(index: GlobalIndex[G]): CFGNode[G] = {
    ???   // TODO: Implement!
  }

  def exception_successor(exception: Expr[G], index: GlobalIndex[G]): CFGNode[G] = {
    ???   // TODO: Implement!
  }

  def break_successor(index: GlobalIndex[G]): CFGNode[G] = {
    ???   // TODO: Implement!
  }

  def continue_successor(index: GlobalIndex[G]): CFGNode[G] = {
    ???   // TODO: Implement!
  }
}
