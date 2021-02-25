package vct.col.rewrite

import hre.lang.System.Output
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr._
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{AbstractRewriter, NameScanner}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * This rewrite pass simplifies expressions of roughly this form:
  * forall i: Int . lower <= i <= higher ==> left {<=|<|==|>|>=} right
  * where left or right is independent of all quantified names
  */
class SimplifyQuantifiedRelations(source: ProgramUnit) extends AbstractRewriter(source) {
  class Bounds(val names: Set[String]) {
    val lowerBounds: mutable.Map[String, Seq[ASTNode]] = mutable.Map()
    val upperBounds: mutable.Map[String, Seq[ASTNode]] = mutable.Map()

    def addLowerBound(name: String, bound: ASTNode): Unit =
      lowerBounds(name) = lowerBounds.getOrElse(name, Seq()) :+ bound

    def addUpperBound(name: String, bound: ASTNode): Unit =
      upperBounds(name) = upperBounds.getOrElse(name, Seq()) :+ bound

    def extremeValue(name: String, maximizing: Boolean): Option[ASTNode] =
      (if(maximizing) upperBounds else lowerBounds).get(name) match {
        case None => None
        case Some(bounds) => Some(SimplifyQuantifiedRelations.this.extremeValue(bounds, !maximizing))
      }

    def selectNonEmpty: Seq[ASTNode] =
      lowerBounds.flatMap {
        case (name, lowerBounds) =>
          upperBounds.getOrElse(name, Seq()).flatMap(upperBound =>
            lowerBounds.map(lowerBound => create.expression(StandardOperator.LTE, lowerBound, upperBound))
          )
      }.toSeq
  }

  def getNames(node: ASTNode): Set[String] = {
    val scanner = new NameScanner()
    node.accept(scanner)
    scanner.accesses
  }

  /**
    * a && b && c --> Seq(a, b, c)
    */
  def getConjuncts(node: ASTNode): Seq[ASTNode] = node match {
    case op: OperatorExpression if op.operator == And =>
      getConjuncts(op.first) ++ getConjuncts(op.second)
    case other => Seq(other)
  }

  def splitSelect(select: ASTNode, main: ASTNode): (Seq[ASTNode], ASTNode) = {
    var left: ArrayBuffer[ASTNode] = ArrayBuffer()
    var right = main

    left ++= getConjuncts(select)

    while(right.isa(StandardOperator.Implies)) {
      left ++= getConjuncts(right.asInstanceOf[OperatorExpression].first)
      right = right.asInstanceOf[OperatorExpression].second
    }

    (left, right)
  }

  def indepOf(names: Set[String], node: ASTNode): Boolean =
    getNames(node).intersect(names).isEmpty

  def isNameIn(names: Set[String], node: ASTNode): Boolean = node match {
    case name: NameExpression if names.contains(name.getName) => true
    case _ => false
  }

  def mapOp(op: StandardOperator, args: Option[ASTNode]*): Option[ASTNode] = {
    if(args.forall(_.isDefined)) {
      Some(create expression(op, args.map(_.get):_*))
    } else {
      None
    }
  }

  /**
   * If we want the maximum/minimum of a list of nodes, that is encoded as an ITE. If we want to compose with our own
   * pass (i.e. have nested foralls) it is nice to be able to transform something like (a < b ? a : b) back to min(a,b)
   * if we encounter it again. We could/should make nodes min/max(a, b, c, ...), but this also works for now.
   */
  val extremeOfListNode: mutable.Map[ASTNode, (Seq[ASTNode], Boolean)] = mutable.Map()

  def extremeValue(values: Seq[ASTNode], maximizing: Boolean): ASTNode = {
    val result = if(values.size == 1) {
      values.head
    } else {
      val preferFirst = if(maximizing) GT else LT
      create.expression(ITE,
        create.expression(preferFirst, values(0), values(1)),
        extremeValue(values(0) +: values.drop(2), maximizing),
        extremeValue(values(1) +: values.drop(2), maximizing),
      )
    }
    extremeOfListNode(result) = (values, maximizing)
    result
  }

  def indepBounds(bounds: Bounds, nodes: ASTNode*): Boolean = {
    nodes.map(getNames(_).intersect(bounds.names)).foldLeft[Either[Unit, Set[String]]](Right(Set.empty)) {
      case (Right(used), nameSet) =>
        if(nameSet.intersect(used).nonEmpty) {
          Left(())
        } else {
          Right(nameSet ++ used)
        }
      case (Left(()), _) => Left(())
    }.isRight
  }

  def extremeValue(bounds: Bounds, node: ASTNode, maximizing: Boolean): Option[ASTNode] = node match {
    case op: OperatorExpression => op.operator match {
      case Plus =>
        if(!indepBounds(bounds, op.first, op.second)) None
        else mapOp(Plus, extremeValue(bounds, op.first, maximizing), extremeValue(bounds, op.second, maximizing))
      case Minus =>
        if(!indepBounds(bounds, op.first, op.second)) None
        else mapOp(Minus, extremeValue(bounds, op.first, maximizing), extremeValue(bounds, op.second, !maximizing))
      case Mult | Div | FloorDiv =>
        val (left, right) = (op.first, op.second)
        if(!indepBounds(bounds, left, right)) return None

        val leftA = extremeValue(bounds, left, maximizing)
        val leftB = extremeValue(bounds, left, !maximizing)
        val rightA = extremeValue(bounds, right, maximizing)
        val rightB = extremeValue(bounds, right, !maximizing)
        val maybeValues = Seq(
          mapOp(op.operator, leftA, rightA),
          mapOp(op.operator, leftB, rightB),
        )
        if(maybeValues.exists(_.isEmpty)) return None
        // We take distinct here in case both sides are constant
        val values = maybeValues.map(_.get).distinct
        Some(extremeValue(values, maximizing))
      case UMinus => extremeValue(bounds, node, !maximizing)
      case ITE if extremeOfListNode.contains(node) =>
        val (nodes, nodeIsMaximizing) = extremeOfListNode(node)

        /* When maximizing == nodeIsMaximizing, e.g.:
         * max_i { max(f(i), g(i)) | bounds }
         * = max(max_i { f(i) | bounds }, max_i { g(i) | bounds })
         *
         * When maximizing == !nodeIsMaximizing, e.g.:
         * max_{i,j} { min(f(i), g(j)) | bounds }
         * = min(max_{i,j} { f(i) }, max_{i,j} { g(j) })
         * check that the arguments to inner function are mutually independent over the bound variables
         */

        if(maximizing != nodeIsMaximizing && !indepBounds(bounds, nodes:_*)) {
          return None
        }

        val extremeNodes = nodes.map(extremeValue(bounds, _, maximizing))
        if(extremeNodes.exists(_.isEmpty)) return None
        Some(extremeValue(extremeNodes.map(_.get), nodeIsMaximizing))
      case _ => None
    }
    case name: NameExpression if bounds.names.contains(name.getName) => bounds.extremeValue(name.getName, maximizing)
    case other: NameExpression => Some(other)
    case const: ConstantExpression => Some(const)
    case _ => None
  }

  /**
    * Try to derive set bounds for all the quantified variables
    * @param decls The names for which to derive bounds
    * @param select The "select" portion of a BindingExpression, e.g. of the form 0 <= i && i < n && 0 <= j && j < m
    * @return When succesful, a map of each name in `decls` to its inclusive lower bound and exclusive upper bound
    */
  def getBounds(decls: Set[String], select: Seq[ASTNode]): Option[Bounds] = {
    val bounds = new Bounds(decls)

    select.foreach {
      case expr: OperatorExpression if Set(LT, LTE, GT, GTE, EQ).contains(expr.operator) =>
        val (quant, op, bound) = if(isNameIn(decls, expr.first) && indepOf(decls, expr.second)) {
          // If the quantified variable is the first argument: keep it as is
          (expr.first, expr.operator, expr.second)
        } else if(isNameIn(decls, expr.second) && indepOf(decls, expr.first)) {
          // If the quantified variable is the second argument: flip the relation
          val op = expr.operator match {
            case LT => GT
            case LTE => GTE
            case GT => LT
            case GTE => LTE
            case EQ => EQ
          }
          (expr.second, op, expr.first)
        } else {
          return None
        }

        val name = quant.asInstanceOf[NameExpression].getName

        op match {
          case LT =>
            bounds.addUpperBound(name, create expression(Minus, bound, create constant 1))
          case LTE =>
            bounds.addUpperBound(name, bound)
          case GT =>
            bounds.addLowerBound(name, create expression(Plus, bound, create constant 1))
          case GTE =>
            bounds.addLowerBound(name, bound)
          case EQ =>
            bounds.addUpperBound(name, bound)
            bounds.addLowerBound(name, bound)
        }
      case OperatorExpression(Member, List(elem, OperatorExpression(RangeSeq, List(low, high)))) =>
        val name = elem match {
          case expr: NameExpression if decls.contains(expr.getName) =>
            expr.getName
          case _ => return None
        }

        if(indepOf(decls, low) && indepOf(decls, high)) {
          bounds.addUpperBound(name, create expression(Minus, high, create constant 1))
          bounds.addLowerBound(name, low)
        } else {
          return None
        }
      case _ => return None
    }

    Some(bounds)
  }

  def rewriteMain(bounds: Bounds, main: ASTNode): Option[ASTNode] = {
    val (left, op, right) = main match {
      case exp: OperatorExpression if Set(LT, LTE, GT, GTE).contains(exp.operator) =>
        (exp.first, exp.operator, exp.second)
      case _ => return None
    }

    /* If one side is independent of the quantified variables, only emit the strongest statement.
     * e.g. forall i: 0<=i<n ==> len > i
     * equivalent to: (0<n) ==> len >= n
     */
    if(indepOf(bounds.names, left)) {
      if(Set(LT, LTE).contains(op)) {
        // constant <= right
        extremeValue(bounds, right, maximizing = false)
          .map(min => create expression(op, left, min))
      } else /* GT, GTE */ {
        // constant >= right
        extremeValue(bounds, right, maximizing = true)
          .map(max => create expression(op, left, max))
      }
    } else if(indepOf(bounds.names, right)) {
      if(Set(LT, LTE).contains(op)) {
        // left <= constant
        extremeValue(bounds, left, maximizing = true)
          .map(max => create expression(op, max, right))
      } else /* GT, GTE */ {
        // left >= constant
        extremeValue(bounds, left, maximizing = false)
          .map(min => create expression(op, min, right))
      }
    } else {
      None
    }
  }

  override def visit(expr: BindingExpression): Unit = {
    expr.binder match {
      case Binder.Forall =>
        val bindings = expr.getDeclarations.map(_.name).toSet
        val (select, main) = splitSelect(rewrite(expr.select), rewrite(expr.main))
        val (indepSelect, potentialBounds) = select.partition(indepOf(bindings, _))
        val bounds = getBounds(bindings, potentialBounds) match {
          case None => super.visit(expr); return
          case Some(bounds) => bounds
        }
        val claim = rewriteMain(bounds, main) match {
          case None => super.visit(expr); return
          case Some(main) => main
        }
        result = create expression(Implies, (indepSelect ++ bounds.selectNonEmpty).reduce(and), claim)
      case _ =>
        super.visit(expr)
    }
  }
}
