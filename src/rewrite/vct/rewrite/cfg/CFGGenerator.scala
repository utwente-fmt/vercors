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
      case label: Label[_] =>
        // For labels, add them to the label map and add them to any goto statements going to that label
        found_labels.addOne((label.decl, cfg_node))
        searched_labels.getOrElse(label.decl, mutable.Set()).map(g => g.successors.addOne(CFGEdge(cfg_node, None)))
      case goto: Goto[_] =>
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
    // Exceptional statements
    case Assign(_, _) => context.indices.head match {
      case AssignmentIndex(a, _) if a == node => sequential_successor(context)
      case _ => evaluate_first(context.enter_scope(node))
    }
    case Goto(lbl) => found_labels.get(lbl.decl) match {
      case Some(node) => mutable.Set(CFGEdge(node, None))
      case None => mutable.Set()
    }
    case Fork(obj) =>   // TODO: Evaluate the obj expression before executing fork
      val run_method: RunMethod[G] = obj.t.asClass.get.cls.decl.declarations.collect{ case r: RunMethod[G] => r }.head
      // Get the successor(s) of the fork statement as well as the new thread, starting with the run method
      sequential_successor(context).addOne(CFGEdge(convert(run_method.body.get, GlobalIndex[G](mutable.Seq()).enter_scope(run_method)), None))
    case Return(result) => handle_expression_container(node, Eval(result)(result.o), context, mutable.Set(CFGEdge(return_successor(context), None)))
    case Throw(obj) => handle_expression_container(node, Eval(obj)(obj.o), context, mutable.Set(CFGEdge(exception_successor(obj, context), None)))
    case Break(label) => label match {
      case Some(ref) => ???  // TODO: Handle break label!
      case None => mutable.Set(CFGEdge(break_successor(context), None))
    }
    case Continue(label) => label match {
      case Some(ref) => ???  // TODO: Handle continue label!
      case None => mutable.Set(CFGEdge(continue_successor(context), None))
    }
    case IndetBranch(branches) =>
      mutable.LinkedHashSet.from(branches.zipWithIndex.map(b => CFGEdge(convert(b._1, context.enter_scope(node, b._2)), None)))
    case s: Switch[_] => handle_switch_statement(s, context.enter_scope(node))
    case CGoto(label) => ???
    case SeqBranch(_, yes, no) => no match {                                        // TODO: What are the conditions here?
      case Some(stmt) => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node, 0)), None), CFGEdge(convert(stmt, context.enter_scope(node, 1)), None))
      case None => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node)), None))
    }
    case SeqLoop(_, _, _) => evaluate_first(context.enter_scope(node))              // TODO: What are the conditions here?
    // Statements that can be categorized into a broader role for control flow analysis
    case ecs: ExpressionContainerStatement[_] => handle_expression_container(node, Eval(ecs.expr)(ecs.o), context, sequential_successor(context))
    case _: ControlContainerStatement[_] => evaluate_first(context.enter_scope(node))
    case _: PurelySequentialStatement[_] => sequential_successor(context)
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
    if (index.has_statement()) mutable.Set(CFGEdge(resolve_index(index), None))
    else sequential_successor(index)
  }

  private def sequential_successor(index: GlobalIndex[G], cond: Option[Expr[G]] = None): mutable.Set[CFGEdge[G]] = {
    if (index.indices.nonEmpty) index.make_step().map(i => CFGEdge(resolve_index(i._1), Utils.and(i._2, cond)))
    else mutable.Set(CFGEdge(CFGTerminal(), cond))
  }

  private def handle_switch_statement(switch: Switch[G], context: GlobalIndex[G]): mutable.Set[CFGEdge[G]] = {
    val switches: Seq[(SwitchCase[G], GlobalIndex[G])] = Utils.find_all_cases(switch.body, context.enter_scope(switch))
    var conds: Seq[(Case[G], Eval[G], GlobalIndex[G])] = Seq()
    var default: Option[(DefaultCase[G], GlobalIndex[G])] = None
    for (s <- switches) {
      s._1 match {
        case c @ Case(pattern) =>
          val cond = Eq(switch.expr, pattern)(c.o)
          // Collect only up to default case
          if (default.isEmpty) conds = conds :+ (c, Eval(cond)(cond.o), s._2)
        case c @ DefaultCase() => default = Some((c, s._2))
      }
    }
    conds = conds.reverse

    var successor_to_previous: mutable.Set[CFGEdge[G]] = default match {
      case Some(t) => mutable.Set(CFGEdge(convert(t._1, t._2), Some(Utils.negate(conds.head._2.expr))))
      case None => sequential_successor(context, Some(Utils.negate(conds.head._2.expr)))
    }

    for ((c, i) <- conds.zipWithIndex) {
      successor_to_previous.addOne(CFGEdge(convert(c._1, c._3), Some(c._2.expr)))
      val node = CFGNode(c._2, successor_to_previous)
      // TODO: Enter node at appropriate index in converted_nodes!
      if (i == conds.size - 1) successor_to_previous = mutable.Set(CFGEdge(node, None))
      else successor_to_previous = mutable.Set(CFGEdge(node, Some(Utils.negate(conds(i + 1)._2.expr))))
    }
    successor_to_previous
  }

  private def return_successor(index: GlobalIndex[G]): CFGEntry[G] =
    resolve_index(index.return_from_call())

  private def exception_successor(exception: Expr[G], index: GlobalIndex[G]): CFGEntry[G] =
    resolve_index(index.handle_exception(exception))

  private def break_successor(index: GlobalIndex[G]): CFGEntry[G] =
    resolve_index(index.handle_break())

  private def continue_successor(index: GlobalIndex[G]): CFGEntry[G] =
    resolve_index(index.continue_innermost_loop())

  private def resolve_index(index: GlobalIndex[G]): CFGEntry[G] = index.resolve() match {
    case Some(stmt) => convert(stmt, index)
    case None => CFGTerminal()
  }
}