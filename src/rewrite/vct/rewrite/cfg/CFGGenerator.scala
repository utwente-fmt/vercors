package vct.rewrite.cfg

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{Ref, DirectRef}

import scala.collection.mutable

case class CFGGenerator[G]() {
  private val found_labels: mutable.Map[LabelDecl[G], CFGNode[G]] = mutable.HashMap[LabelDecl[G], CFGNode[G]]()
  private val searched_labels: mutable.Map[LabelDecl[G], mutable.Set[CFGNode[G]]] = mutable.HashMap[LabelDecl[G], mutable.Set[CFGNode[G]]]()
  private val converted_nodes: mutable.Map[GlobalIndex[G], CFGNode[G]] = mutable.HashMap[GlobalIndex[G], CFGNode[G]]()

  def generate(entry: InstanceMethod[G]): CFGNode[G] = {
    convert(entry.body.get, GlobalIndex[G])
  }

  def convert(node: Statement[G], context: GlobalIndex[G]): CFGNode[G] = {
    // If a node has already been visited, then it should not be created again
    if (converted_nodes.contains(context)) return converted_nodes(context)
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
    converted_nodes.addOne((context, cfg_node))
    cfg_node
  }

  def find_successors(node: Statement[G], context: GlobalIndex[G]): mutable.Set[CFGNode[G]] = node match {
    case PVLBranch(branches) => {
      val eval: Eval[G] = Eval(branches.head._1)(branches.head._1.o)
      mutable.Set(convert(eval, context.enter_scope(node)))
    }
    case PVLLoop(init, _, _, _, _) =>
      mutable.Set(convert(init, context.enter_scope(node)))
    // NonExecutableStatement
    case LocalDecl(_) => sequential_successor(context)
    case SpecIgnoreStart() => sequential_successor(context)     // TODO: What is this?
    case SpecIgnoreEnd() => sequential_successor(context)
    // NormallyCompletingStatement
    case Assign(target, value) => sequential_successor(context)  // TODO: Implement support for multiple expressions in container statement
    case Send(_, _, res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Recv(_) => sequential_successor(context)
    case DefaultCase() => sequential_successor(context)
    case Case(pattern) => sequential_successor(context)   // TODO: Handle expression side effects in switch case conditions
    case Label(_, stat) =>
      mutable.Set(convert(stat, context.enter_scope(node)))
    case Goto(lbl) => {
      val found_node: Option[CFGNode[G]] = found_labels.get(lbl.decl)
      found_node match {
        case Some(node) => mutable.Set(node)
        case None => mutable.Set()
      }
    }
    case Exhale(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Assert(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Refute(assn) => handle_expression_container(node, Eval(assn)(assn.o), context, sequential_successor(context))
    case Inhale(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Assume(assn) => handle_expression_container(node, Eval(assn)(assn.o), context, sequential_successor(context))
    case Instantiate(_, out) => handle_expression_container(node, Eval(out)(out.o), context, sequential_successor(context))
    case Wait(_) => sequential_successor(context)
    case Notify(_) => sequential_successor(context)
    case Fork(obj) => {
      val run_method: RunMethod[G] = obj.t.asClass.get.cls.decl.declarations.collect { case r: RunMethod[G] => r }.head
      sequential_successor(context).addOne(convert(run_method.body.get, context.enter_scope(run_method)))
    }
    case Join(_) => sequential_successor(context)
    case Lock(_) => sequential_successor(context)
    case Unlock(_) => sequential_successor(context)
    case Commit(_) => sequential_successor(context)
    case Fold(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Unfold(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case WandApply(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Havoc(_) => sequential_successor(context)
    case FramedProof(pre, body, post) =>
      ???   // TODO: Support multiple expressions in a container statement
    case Extract(contractedStatement) =>
      mutable.Set(convert(contractedStatement, context.enter_scope(node)))
    // ExceptionalStatement
    case Eval(_) => sequential_successor(context.enter_scope(node))   // TODO: Is this correct?
    case Return(result) => handle_expression_container(node, Eval(result)(result.o), context, mutable.Set(return_successor(context)))
    case Throw(obj) => mutable.Set(exception_successor(obj, context))
    case Break(label) => label match {
      case Some(ref) => ???  // TODO: Handle break label!
      case None => mutable.Set(break_successor(context))
    }
    case Continue(label) => label match {
      case Some(ref) => ???  // TODO: Handle continue label!
      case None => mutable.Set(continue_successor(context))
    }
    // InvocationStatement
    case InvokeProcedure(ref, args, _, _, _, _) => {
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
    // CompositeStatement
    case Block(statements) =>
      mutable.Set(convert(statements.head, context.enter_scope(node)))
    case Scope(_, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case Branch(branches) => {
      val eval: Eval[G] = Eval(branches.head._1)(branches.head._1.o)
      mutable.Set(convert(eval, context.enter_scope(node)))
    }
    case IndetBranch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1, context.enter_scope(node, b._2))))
    case Switch(expr, body) => {
      val cases: mutable.Set[(SwitchCase[G], GlobalIndex[G])] = Utils.find_all_cases(body, context.enter_scope(node))
      cases.map(t => convert(t._1, t._2))   // TODO: Handle side effects in switch statement conditions
    }
    case Loop(init, _, _, _, _) =>
      mutable.Set(convert(init, context.enter_scope(node)))
    case RangedFor(_, _, body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case TryCatchFinally(body, _, _) =>
      mutable.Set(convert(body, context.enter_scope(node)))
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
    case CGoto(label) => ???    // I'm not dealing with string labels
    // CPPStatement
    case CPPDeclarationStatement(_) => sequential_successor(context)
    case CPPLifetimeScope(body) =>
      mutable.Set(convert(body, context.enter_scope(node)))
    case JavaLocalDeclarationStatement(_) => sequential_successor(context)
    // SilverStatement
    case SilverNewRef(_, _) => sequential_successor(context)
    case SilverFieldAssign(_, _, value) => handle_expression_container(node, Eval(value)(value.o), context, sequential_successor(context))
    case SilverLocalAssign(_, value) => handle_expression_container(node, Eval(value)(value.o), context, sequential_successor(context))
    // OTHER
    case PVLCommunicate(_, _) => sequential_successor(context)
    case PVLSeqAssign(_, _, value) => handle_expression_container(node, Eval(value)(value.o), context, sequential_successor(context))
    case Communicate(_, _) => sequential_successor(context)
    case SeqAssign(_, _, value) => handle_expression_container(node, Eval(value)(value.o), context, sequential_successor(context))
    case UnresolvedSeqBranch(branches) =>
      mutable.Set(branches.zipWithIndex.map(b => convert(b._1._2, context.enter_scope(node, b._2))))
    case UnresolvedSeqLoop(cond, _, _) =>
      mutable.Set(convert(Eval(cond)(cond.o), context.enter_scope(node)))
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

  private def handle_expression_container(statement: Statement[G],
                                          expression: Eval[G],
                                          context: GlobalIndex[G],
                                          successors_to_statement: mutable.Set[CFGNode[G]]): mutable.Set[CFGNode[G]] = {
    context.indices.head match {
      case ExpressionContainerIndex(stmt, 1) if stmt == statement => successors_to_statement
      case _ => mutable.Set(convert(expression, context.enter_scope(expression)))     // TODO: Handle containers with multiple expressions
    }
  }

  private def sequential_successor(index: GlobalIndex[G]): mutable.Set[CFGNode[G]] =
    mutable.Set(index.make_step().map(i => convert(i.resolve(), i)))

  private def return_successor(index: GlobalIndex[G]): CFGNode[G] = {
    val new_index: GlobalIndex[G] = index.return_from_call()
    convert(new_index.resolve(), new_index)
  }

  private def exception_successor(exception: Expr[G], index: GlobalIndex[G]): CFGNode[G] = {
    val new_index = index.handle_exception(exception)
    // Terminate on unhandled exception
    if (new_index.indices.isEmpty) return CFGNode(exception, mutable.Set())
    convert(new_index.resolve(), new_index)
  }

  private def break_successor(index: GlobalIndex[G]): CFGNode[G] = {
    val new_index = index.handle_break()
    convert(new_index.resolve(), new_index)
  }

  private def continue_successor(index: GlobalIndex[G]): CFGNode[G] = {
    val new_index = index.continue_innermost_loop()
    convert(new_index.resolve(), new_index)
  }
}