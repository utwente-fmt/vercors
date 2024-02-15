package vct.rewrite.cfg

import vct.col.ast._

import scala.collection.mutable

case class CFGGenerator[G]() {
  private val found_labels: mutable.Map[LabelDecl[G], CFGNode[G]] = mutable.HashMap[LabelDecl[G], CFGNode[G]]()
  private val searched_labels: mutable.Map[LabelDecl[G], mutable.Set[CFGNode[G]]] = mutable.HashMap[LabelDecl[G], mutable.Set[CFGNode[G]]]()
  private val converted_nodes: mutable.Map[GlobalIndex[G], CFGNode[G]] = mutable.HashMap[GlobalIndex[G], CFGNode[G]]()

  def generate(entry: InstanceMethod[G]): CFGNode[G] = {
    convert(entry.body.get, GlobalIndex[G](mutable.Seq(InitialIndex(entry))))
  }

  private def convert(node: Statement[G], context: GlobalIndex[G]): CFGNode[G] = {
    // If a node has already been visited, then it should not be created again
    if (converted_nodes.contains(context)) return converted_nodes(context)
    // Create new node with its successors (if possible)
    val cfg_node: CFGNode[G] = CFGNode(node, mutable.Set())
    converted_nodes.addOne((context, cfg_node))
    cfg_node.successors.addAll(find_successors(node, context))
    // Handle labels and goto statements
    node match {
      case label: Label[G] => {
        // For labels, add them to the label map and add them to any goto statements going to that label
        found_labels.addOne((label.decl, cfg_node))
        searched_labels.getOrElse(label.decl, mutable.Set()).map(g => g.successors.addOne(CFGEdge(cfg_node, None)))
      }
      case goto: Goto[G] =>
        // For goto statements, if the label could not be resolved, add the statement to the waiting list for the right label
        if (cfg_node.successors.isEmpty) {
          if (searched_labels.contains(goto.lbl.decl)) searched_labels(goto.lbl.decl).addOne(cfg_node)
          else searched_labels.addOne((goto.lbl.decl, mutable.Set(cfg_node)))
        }
      case _ =>
    }
    cfg_node
  }

  private def find_successors(node: Statement[G], context: GlobalIndex[G]): mutable.Set[CFGEdge[G]] = node match {
    // TODO: Can these be simplified using evaluate_first()?
    case PVLBranch(_) => evaluate_first(context.enter_scope(node))
    case PVLLoop(_, _, _, _, _) => evaluate_first(context.enter_scope(node))
    // NonExecutableStatement
    case LocalDecl(_) => sequential_successor(context)
    case SpecIgnoreStart() => sequential_successor(context)     // TODO: What is this?
    case SpecIgnoreEnd() => sequential_successor(context)
    // NormallyCompletingStatement
    case Assign(_, _) => context.indices.head match {
      case AssignmentIndex(a, _) if a == node => sequential_successor(context)
      case _ => evaluate_first(context.enter_scope(node))
    }
    case Send(_, _, res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Recv(_) => sequential_successor(context)
    case DefaultCase() => sequential_successor(context)
    case Case(pattern) => sequential_successor(context)   // TODO: Handle expression side effects in switch case conditions
    case Label(_, _) => evaluate_first(context.enter_scope(node))
    case Goto(lbl) => found_labels.get(lbl.decl) match {
      case Some(node) => mutable.Set(CFGEdge(node, None))
      case None => mutable.Set()
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
      val run_method: RunMethod[G] = obj.t.asClass.get.cls.decl.declarations.collect{ case r: RunMethod[G] => r }.head
      sequential_successor(context).addOne(CFGEdge(convert(run_method.body.get, context.enter_scope(run_method)), None))
    }
    case Join(_) => sequential_successor(context)
    case Lock(_) => sequential_successor(context)
    case Unlock(_) => sequential_successor(context)
    case Commit(_) => sequential_successor(context)
    case Fold(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Unfold(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case WandApply(res) => handle_expression_container(node, Eval(res)(res.o), context, sequential_successor(context))
    case Havoc(_) => sequential_successor(context)
    case FramedProof(_, _, _) => evaluate_first(context.enter_scope(node))
    case Extract(_) => evaluate_first(context.enter_scope(node))
    // ExceptionalStatement
    case Eval(_) => evaluate_first(context.enter_scope(node))
    case Return(result) => handle_expression_container(node, Eval(result)(result.o), context, mutable.Set(CFGEdge(return_successor(context), None)))
    case Throw(obj) => mutable.Set(CFGEdge(exception_successor(obj, context), None))
    case Break(label) => label match {
      case Some(ref) => ???  // TODO: Handle break label!
      case None => mutable.Set(CFGEdge(break_successor(context), None))
    }
    case Continue(label) => label match {
      case Some(ref) => ???  // TODO: Handle continue label!
      case None => mutable.Set(CFGEdge(continue_successor(context), None))
    }
    // InvocationStatement
    case InvokeProcedure(_, _, _, _, _, _) => evaluate_first(context.enter_scope(node))
    case InvokeConstructor(_, _, _, _, _, _, _) => evaluate_first(context.enter_scope(node))
    case InvokeMethod(_, _, _, _, _, _, _) => evaluate_first(context.enter_scope(node))
    // CompositeStatement
    case Block(_) => evaluate_first(context.enter_scope(node))
    case Scope(_, _) => evaluate_first(context.enter_scope(node))
    case Branch(_) => evaluate_first(context.enter_scope(node))
    case IndetBranch(branches) =>
      mutable.LinkedHashSet.from(branches.zipWithIndex.map(b => CFGEdge(convert(b._1, context.enter_scope(node, b._2)), None)))
    case Switch(expr, body) =>    // TODO: Add conditions to switch statement transitions
      Utils.find_all_cases(body, context.enter_scope(node)).map(t => CFGEdge(convert(t._1, t._2), None))   // TODO: Handle side effects in switch statement conditions
    case Loop(_, _, _, _, _) => evaluate_first(context.enter_scope(node))
    case RangedFor(_, _, _) => evaluate_first(context.enter_scope(node))
    case TryCatchFinally(_, _, _) => evaluate_first(context.enter_scope(node))
    case Synchronized(_, _) => evaluate_first(context.enter_scope(node))
    case ParInvariant(_, _, _) => evaluate_first(context.enter_scope(node))       // TODO: Expression side effects
    case ParAtomic(_, _) => evaluate_first(context.enter_scope(node))             // TODO: Expression side effects
    case ParBarrier(_, _, _, _, _) => evaluate_first(context.enter_scope(node))   // TODO: Expression side effects
    case ParStatement(_) => sequential_successor(context)
    case VecBlock(_, _, _, _) => evaluate_first(context.enter_scope(node))        // TODO: Expression side effects
    case WandPackage(_, _) => evaluate_first(context.enter_scope(node))           // TODO: Expression side effects
    case ModelDo(_, _, _, _, _) => evaluate_first(context.enter_scope(node))      // TODO: Expression side effects
    // CStatement
    case CDeclarationStatement(_) => sequential_successor(context)
    case CGoto(label) => ???    // I'm not dealing with string labels
    // CPPStatement
    case CPPDeclarationStatement(_) => sequential_successor(context)
    case CPPLifetimeScope(_) => evaluate_first(context.enter_scope(node))
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
      mutable.LinkedHashSet.from(branches.zipWithIndex.map(b => CFGEdge(convert(b._1._2, context.enter_scope(node, b._2)), None)))
    case UnresolvedSeqLoop(_, _, _) => evaluate_first(context.enter_scope(node))
    case SeqBranch(_, yes, no) => no match {                                        // TODO: What are the conditions here?
      case Some(stmt) => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node, 0)), None), CFGEdge(convert(stmt, context.enter_scope(node, 1)), None))
      case None => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node)), None))
    }
    case SeqLoop(_, _, _) => evaluate_first(context.enter_scope(node))              // TODO: What are the conditions here?
    case VeyMontAssignExpression(_, _) => evaluate_first(context.enter_scope(node))
    case CommunicateX(_, _, _, _) => evaluate_first(context.enter_scope(node))
  }

  private def handle_expression_container(statement: Statement[G],
                                          expression: Eval[G],
                                          context: GlobalIndex[G],
                                          successors_to_statement: mutable.Set[CFGEdge[G]]): mutable.Set[CFGEdge[G]] = {
    context.indices.head match {
      case ExpressionContainerIndex(stmt, 1) if stmt == statement => successors_to_statement
      case _ => mutable.Set(CFGEdge(convert(expression, context.enter_scope(expression)), None))
    }
  }

  private def evaluate_first(index: GlobalIndex[G]): mutable.Set[CFGEdge[G]] = {
    if (index.has_statement()) mutable.Set(CFGEdge(convert(index.resolve(), index), None))
    else sequential_successor(index)
  }

  private def sequential_successor(index: GlobalIndex[G]): mutable.Set[CFGEdge[G]] =
    index.make_step().map(i => CFGEdge(convert(i._1.resolve(), i._1), i._2))

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