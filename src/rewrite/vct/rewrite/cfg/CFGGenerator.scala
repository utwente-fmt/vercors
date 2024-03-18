package vct.rewrite.cfg

import vct.col.ast._

import scala.collection.mutable

case class CFGGenerator[G](hide_composite_statements: Boolean = true) {
  private val found_labels: mutable.Map[LabelDecl[G], CFGNode[G]] = mutable.HashMap[LabelDecl[G], CFGNode[G]]()
  private val searched_labels: mutable.Map[LabelDecl[G], mutable.Set[CFGNode[G]]] = mutable.HashMap[LabelDecl[G], mutable.Set[CFGNode[G]]]()
  private val converted_nodes: mutable.Map[GlobalIndex[G], CFGNode[G]] = mutable.HashMap[GlobalIndex[G], CFGNode[G]]()

  def generate(entry: InstanceMethod[G]): CFGNode[G] = {
    convert(entry.body.get, GlobalIndex[G](mutable.Seq(InitialIndex(entry))))
  }

  private def convert(node: Statement[G], context: GlobalIndex[G]): CFGNode[G] = {
    // If a node has already been visited, then it should not be created again
    if (converted_nodes.contains(context)) return converted_nodes(context)
    // Convert to CFG node depending on type of statement
    node match {
      // If a statement contains an expression, the expression(s) must be evaluated before the statement
      case assign: Assign[_] => assign_to_cfg(assign, context)
      case stmt: ExpressionContainerStatement[_] => expression_container_to_cfg(stmt, context)
      // Handle labels and gotos, since those can occur in arbitrary order
      case label: Label[_] =>
        val cfg_node: CFGNode[G] = statement_to_cfg(node, context)
        // For labels, add them to the label map and add them to any goto statements going to that label
        found_labels.addOne((label.decl, cfg_node))
        searched_labels.getOrElse(label.decl, mutable.Set()).foreach(g => g.successors.addOne(CFGEdge(cfg_node, None)))
        cfg_node
      case goto: Goto[_] =>
        val cfg_node: CFGNode[G] = statement_to_cfg(node, context)
        // For goto statements, if the label could not be resolved, add the statement to the waiting list for the right label
        if (cfg_node.successors.isEmpty) {
          if (searched_labels.contains(goto.lbl.decl)) searched_labels(goto.lbl.decl).addOne(cfg_node)
          else searched_labels.addOne((goto.lbl.decl, mutable.Set(cfg_node)))
        }
        cfg_node
      // Leave all container statements except for invocations out of the CFG with two exceptions: Expressions and the
      // first scope of a run method must remain so that the run method can later still be identified
      case _: InvocationStatement[_] => statement_to_cfg(node, context)
      case c: ControlContainerStatement[_] if hide_composite_statements && !context.indices.head.isInstanceOf[RunMethodIndex[_]] =>
        val new_context = context.enter_scope(c)
        // If the new scope is empty (e.g. expression evaluation with no contained statements), transform it normally,
        // since this is the only node that can represent this part of the CFG
        if (!new_context.has_statement()) statement_to_cfg(c, context)
        else convert(new_context.resolve().get, new_context)
      // Any non-special statement is simply converted to a CFG node
      case _ => statement_to_cfg(node, context)
    }
  }

  private def statement_to_cfg(stmt: Statement[G], context: GlobalIndex[G]): CFGNode[G] = {
    val cfg_node: CFGNode[G] = CFGNode(stmt, mutable.Set())
    converted_nodes.addOne((context, cfg_node))
    cfg_node.successors.addAll(find_successors(stmt, context))
    cfg_node
  }

  private def assign_to_cfg(assign: Assign[G], context: GlobalIndex[G]): CFGNode[G] = context.indices.head match {
    case AssignmentIndex(a, _) if a == assign => statement_to_cfg(assign, context)
    case _ => convert(Eval(assign.target)(assign.target.o), context.enter_scope(assign))
  }

  private def expression_container_to_cfg(stmt: ExpressionContainerStatement[G], context: GlobalIndex[G]): CFGNode[G] = context.indices.head match {
    case ExpressionContainerIndex(s, _) if s == stmt => statement_to_cfg(stmt, context)
    case _ => convert(Eval(stmt.expr)(stmt.expr.o), context.enter_scope(stmt))
  }

  private def find_successors(node: Statement[G], context: GlobalIndex[G]): mutable.Set[CFGEdge[G]] = node match {
    // Exceptional statements
    // Statements that can jump to another part of the program
    case Goto(lbl) => found_labels.get(lbl.decl) match {
      case Some(node) => mutable.Set(CFGEdge(node, None))
      case None => mutable.Set()
    }
    case CGoto(label) => ???
    // Statements that simultaneously affect other threads
    case Fork(obj) =>
      val run_method: RunMethod[G] = obj.t.asClass.get.cls.decl.declarations.collect{ case r: RunMethod[G] => r }.head
      // Get the successor(s) of the fork statement as well as the new thread, starting with the run method
      sequential_successor(context).addOne(CFGEdge(convert(run_method.body.get, GlobalIndex[G](mutable.Seq()).enter_scope(run_method)), None))
    // Statements that jump out of the current control flow context
    case Return(_) => return_successors(context)
    case Throw(obj) => mutable.Set(CFGEdge(exception_successor(obj, context), None))
    case Break(label) => label match {
      case Some(ref) => ???  // TODO: Handle break label!
      case None => mutable.Set(CFGEdge(break_successor(context), None))
    }
    case Continue(label) => label match {
      case Some(ref) => ???  // TODO: Handle continue label!
      case None => mutable.Set(CFGEdge(continue_successor(context), None))
    }
    // Irregular control container statements
    case IndetBranch(branches) =>
      mutable.LinkedHashSet.from(branches.zipWithIndex.map(b => CFGEdge(convert(b._1, context.enter_scope(node, b._2)), None)))
    case s: Switch[_] => handle_switch_statement(s, context.enter_scope(node))
    case SeqBranch(_, yes, no) => no match {
      case Some(stmt) => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node, 0)), None), CFGEdge(convert(stmt, context.enter_scope(node, 1)), None))
      case None => mutable.Set(CFGEdge(convert(yes, context.enter_scope(node)), None))
    }
    // Assign statements cannot be easily categorized because they contain two expressions
    case Assign(_, _) => sequential_successor(context)
    // Other statements that can be categorized into a broader role for control flow analysis
    case _: ExpressionContainerStatement[_] => sequential_successor(context)
    case _: ControlContainerStatement[_] => evaluate_first(context.enter_scope(node))
    case _: PurelySequentialStatement[_] => sequential_successor(context)
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
      // TODO: Should this node be added to the explored nodes? Probably not, as it is a generated node?
      if (i == conds.size - 1) successor_to_previous = mutable.Set(CFGEdge(node, None))
      else successor_to_previous = mutable.Set(CFGEdge(node, Some(Utils.negate(conds(i + 1)._2.expr))))
    }
    successor_to_previous
  }

  private def return_successors(index: GlobalIndex[G]): mutable.Set[CFGEdge[G]] =
    index.return_from_call().map(t => CFGEdge(resolve_index(t._1), t._2))

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