package vct.col.rewrite

import hre.lang.System.{Output, Warning}
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.constant.{ConstantExpression, StructValue}
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

case class UnrecognizedExpression(node: ASTNode) extends Exception

object Triggers {
  val allowedInTrigger = Set(Scale, /*sequence*/ Subscript, Member, Size, UPlus, Head, Tail, SubSet, SubSetEq, Member, Old, RangeSeq, FoldPlus)
  // Minus is handled specially because we have no specific set minus operator. See below.
  val forbiddenInTrigger = Set(UMinus, Plus, Mult, FloorDiv, Div, Mod, And, Or, Not, Implies, IFF, EQ, NEQ, GT, GTE, LT, LTE, ITE, Star, Wand, Perm, Value)

  /**
   * Collect potentially admissible patterns from an expression
   * @param node the expression to collect
   * @return A set of viable patterns, and whether the node itself may be contained in a pattern
   */
  def collectPatterns(node: ASTNode): (Set[ASTNode], Boolean) = node match {
    case NameExpression(_, reserved, NameExpressionKind.Reserved) => reserved match {
      case ASTReserved.Result =>
        (Set(), true)
      case ASTReserved.FullPerm | ASTReserved.NoPerm | ASTReserved.ReadPerm | ASTReserved.Null =>
        (Set(), false)
      case _ =>
        throw UnrecognizedExpression(node)
    }
    case NameExpression(_, _, _) =>
      (Set(), true)
    case ConstantExpression(_) =>
      (Set(), false)
    case MethodInvokation(_, _, _, args) =>
      val childPatterns = args.map(collectPatterns)
      val childOK = childPatterns.forall(_._2)
      val myPattern = if(childOK) Set(node) else Set()
      (childPatterns.map(_._1).foldLeft(Set[ASTNode]())(_ ++ _) ++ myPattern, childOK)
    case e @ OperatorExpression(op, args) =>
      val isSetMinus = op == StandardOperator.Minus && e.getType != null && (e.getType.isPrimitive(PrimitiveSort.Set) || e.getType.isPrimitive(PrimitiveSort.Bag))
      // Assuming there are no other uses of binary minus
      val isNumericMinus = op == StandardOperator.Minus && !isSetMinus

      if (allowedInTrigger.contains(op) || isSetMinus) {
        val childPatterns = args.map(collectPatterns)
        val childOK = childPatterns.forall(_._2)
        val myPattern = if(childOK) Set(node) else Set()
        (childPatterns.foldLeft(Set[ASTNode]())(_ ++ _._1) ++ myPattern, childOK)
      } else if (forbiddenInTrigger.contains(op) || isNumericMinus) {
        (args.map(collectPatterns).foldLeft(Set[ASTNode]())(_ ++ _._1), false)
      } else {
        throw UnrecognizedExpression(node)
      }
    case Dereference(obj, _) =>
      collectPatterns(obj) match {
        case (pats, false) =>
          (pats, false)
        case (pats, true) =>
          (pats + node, true)
      }
    case StructValue(_, _, xs) =>
      (xs.map(collectPatterns).foldLeft(Set[ASTNode]())(_ ++ _._1), false)
    case BindingExpression(_, _, _, _, select, main) =>
      (Option(select).map(collectPatterns(_)._1).getOrElse(Set()) ++ collectPatterns(main)._1,
        false)
    case _ =>
      throw UnrecognizedExpression(node)
  }

  def mentions(node: ASTNode): Set[String] = node match {
    case NameExpression(name, _, _) => Set(name)
    case ConstantExpression(_) => Set()
    case MethodInvokation(_, _, _, args) => args.map(mentions).foldLeft(Set[String]())(_ ++ _)
    case OperatorExpression(_, args) => args.map(mentions).foldLeft(Set[String]())(_ ++ _)
    case Dereference(obj, _) => mentions(obj)
  }

  def contains(container: ASTNode, element: ASTNode): Boolean =
    container == element || (container match {
      case _: NameExpression | _: ConstantExpression => false
      case MethodInvokation(_, _, _, args) => args.exists(contains(_, element))
      case OperatorExpression(_, args) => args.exists(contains(_, element))
      case Dereference(obj, _) => contains(obj, element)
      case StructValue(_, _, values) => values.exists(contains(_, element))
    })

  def powerset[T](x: Set[T]): Seq[Set[T]] = {
    if(x.isEmpty) {
      Seq(Set())
    } else {
      val tail = powerset(x.tail)
      tail ++ tail.map(_ + x.head)
    }
  }

  def computeTriggers(names: Set[String], node: ASTNode): Seq[Set[ASTNode]] = {
    val patterns = collectPatterns(node)._1.filterNot(mentions(_).intersect(names).isEmpty)
    val patternsNoDirectChild = patterns.filter {
      case Dereference(obj, _) => !patterns.contains(obj)
      case OperatorExpression(Size, List(xs)) => !patterns.contains(xs)
      case MethodInvokation(_, _, "alen", Seq(arr)) => !patterns.contains(arr)
      case _ => true
    }

    var triggers = powerset(patternsNoDirectChild)
    triggers = triggers.filter(_.foldLeft(Set[String]())(_ ++ mentions(_)).intersect(names) == names)
    triggers = triggers.filter(set => set.forall(big => set.forall(small => big == small || !contains(big, small))))
    triggers
  }

  def tryComputeTrigger(decls: Array[DeclarationStatement], cond: ASTNode, body: ASTNode): Either[Set[ASTNode], Set[Set[ASTNode]]] = {
    val names = decls.map(_.name).toSet

    try {
      computeTriggers(names, new OperatorExpression(StandardOperator.Implies, Array(cond, body))) match {
        case Seq() => Left(Set())
        case Seq(set) => Right(Set(set))
        case triggers => body match {
          case op@OperatorExpression(EQ | NEQ | LT | GT | LTE | GTE, List(left, right)) =>
            val leftTriggers = triggers.filter(_.forall(contains(left, _)))
            val rightTriggers = triggers.filter(_.forall(contains(right, _)))
            if (leftTriggers.size == 1 && rightTriggers.size == 1) {
              // Allow relational operators at the top if both sides generate exactly one trigger
              Right((leftTriggers ++ rightTriggers).toSet)
            } else if (leftTriggers.size == 1 && op.operator == EQ) {
              Right(leftTriggers.toSet)
            } else {
              Left(triggers.flatten.toSet)
            }
          case _ => Left(triggers.flatten.toSet)
        }
      }
    } catch {
      case unrecognized: UnrecognizedExpression =>
        Warning("Cannot determine whether this expression may or may not occur in a trigger: %s", unrecognized.node)
        Left(Set())
    }
  }
}

/**
  * Tries to add triggers to binding expressions that have none using some heuristics.
  */
case class Triggers(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(expr: BindingExpression): Unit = {
    expr.binder match {
      case Binder.Forall | Binder.Star if expr.triggers == null || expr.triggers.isEmpty =>
        val select = expr.select
        val main = expr.main
        Triggers.tryComputeTrigger(expr.getDeclarations, select, main) match {
          case Right(trigger) =>
            result = create binder(
              expr.binder,
              expr.result_type,
              expr.getDeclarations,
              trigger.map(_.toArray).toArray,
              rewrite(select),
              rewrite(main)
            )
          case Left(triggers) =>
            if (triggers.isEmpty) {
              hre.lang.System.Warning("Could not find a trigger for an expression", expr.getOrigin)
              hre.lang.System.Warning("Location: %s", expr.getOrigin)
              hre.lang.System.Warning("Expression: %s", expr)
            } else {
              hre.lang.System.Warning("More than one trigger found for an expression")
              hre.lang.System.Warning("Location: %s", expr.getOrigin)
              hre.lang.System.Warning("Expression: %s", expr)
              hre.lang.System.Warning("Possible triggers:")
              triggers.foreach(t => {
                hre.lang.System.Warning("- %s", t)
              })
            }
            result = create binder(
              expr.binder,
              expr.result_type,
              expr.getDeclarations,
              null,
              rewrite(select),
              rewrite(main)
            )
        }
      case _ =>
        super.visit(expr)
    }
  }
}
