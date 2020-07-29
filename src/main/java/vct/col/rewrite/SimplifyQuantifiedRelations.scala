package vct.col.rewrite

import java.util

import vct.col.ast.`type`.Type
import vct.col.ast.expr.StandardOperator.{And, Div, EQ, FloorDiv, GT, GTE, ITE, Implies, LT, LTE, Member, Minus, Mult, Plus, RangeSeq, UMinus}
import vct.col.ast.expr._
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{AbstractRewriter, NameScanner}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * This rewrite pass simplifies expressions of roughly this form:
  * forall i: Int . lower <= i <= higher ==> left {<=|<|==|>|>=} right
  * where left or right is independent of all quantified names
  */
class SimplifyQuantifiedRelations(source: ProgramUnit) extends AbstractRewriter(source) {
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

  def extremeValue(values: Seq[ASTNode], maximizing: Boolean): ASTNode = {
    if(values.size == 1) {
      values.head
    } else {
      val preferFirst = if(maximizing) GT else LT
      create.expression(ITE,
        create.expression(preferFirst, values(0), values(1)),
        extremeValue(values(0) +: values.slice(2, values.size), maximizing),
        extremeValue(values(1) +: values.slice(2, values.size), maximizing),
      )
    }
  }

  def extremeValue(bounds: Map[String, (ASTNode, ASTNode)], node: ASTNode, maximizing: Boolean): Option[ASTNode] = node match {
    case op: OperatorExpression => op.operator match {
      case Plus => mapOp(Plus, extremeValue(bounds, op.first, maximizing), extremeValue(bounds, op.second, maximizing))
      case Minus => mapOp(Minus, extremeValue(bounds, op.first, maximizing), extremeValue(bounds, op.second, !maximizing))
      case Mult | Div | FloorDiv =>
        // Either side can flip the sign, so any combinating of maxima/minima can be the extreme value
        val leftA = extremeValue(bounds, op.first, maximizing)
        val leftB = extremeValue(bounds, op.first, !maximizing)
        val rightA = extremeValue(bounds, op.second, maximizing)
        val rightB = extremeValue(bounds, op.second, !maximizing)
        val maybeValues = Seq(
          mapOp(op.operator, leftA, rightA),
          mapOp(op.operator, leftA, rightB),
          mapOp(op.operator, leftB, rightA),
          mapOp(op.operator, leftB, rightB),
        )
        if(maybeValues.exists(_.isEmpty)) return None
        // We do take distinct here in case both/one side is constant
        val values = maybeValues.map(_.get).distinct
        Some(extremeValue(values, maximizing))
      case UMinus => extremeValue(bounds, node, !maximizing)
      case _ => None
    }
    case name: NameExpression if bounds.contains(name.getName) =>
      Some(if(maximizing) create expression(Minus, bounds(name.getName)._2, create constant 1) else bounds(name.getName)._1)
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
  def getBounds(decls: Array[String], select: ASTNode): Option[Map[String, (ASTNode, ASTNode)]] = {
    var lowerBounds = mutable.Map[String, ASTNode]()
    var upperBounds = mutable.Map[String, ASTNode]()

    getConjuncts(select).foreach {
      case expr: OperatorExpression if Set(LT, LTE, GT, GTE, EQ).contains(expr.operator) =>
        val (quant, op, bound) = if(isNameIn(decls.toSet, expr.first) && indepOf(decls.toSet, expr.second)) {
          // If the quantified variable is the first argument: keep it as is
          (expr.first, expr.operator, expr.second)
        } else if(isNameIn(decls.toSet, expr.second) && indepOf(decls.toSet, expr.first)) {
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
            if(upperBounds.contains(name)) return None
            upperBounds += name -> bound
          case LTE =>
            if(upperBounds.contains(name)) return None
            upperBounds += name -> (create expression(Plus, bound, create constant 1))
          case GT =>
            if(lowerBounds.contains(name)) return None
            lowerBounds += name -> (create expression(Minus, bound, create constant 1))
          case GTE =>
            if(lowerBounds.contains(name)) return None
            lowerBounds += name -> bound
          case EQ =>
            if(upperBounds.contains(name)) return None
            if(lowerBounds.contains(name)) return None
            upperBounds += name -> bound
            lowerBounds += name -> bound
        }
      case OperatorExpression(Member, List(elem, OperatorExpression(RangeSeq, List(low, high)))) =>
        val name = elem match {
          case expr: NameExpression if decls.contains(expr.getName) =>
            expr.getName
          case _ => return None
        }

        if(indepOf(decls.toSet, low) && indepOf(decls.toSet, high)) {
          if(upperBounds.contains(name)) return None
          if(lowerBounds.contains(name)) return None
          upperBounds += name -> high
          lowerBounds += name -> low
        } else {
          return None
        }
      case _ => return None
    }

    if(lowerBounds.keySet == decls.toSet && upperBounds.keySet == decls.toSet) {
      Some(decls.map(decl => decl -> (lowerBounds(decl), upperBounds(decl))).toMap)
    } else {
      None
    }
  }

  def rewriteMain(bounds: Map[String, (ASTNode, ASTNode)], main: ASTNode): Option[ASTNode] = {
    val (left, op, right) = main match {
      case exp: OperatorExpression if Set(LT, LTE, GT, GTE).contains(exp.operator) =>
        (exp.first, exp.operator, exp.second)
      case _ => return None
    }

    val names = bounds.keySet

    /* If one side is independent of the quantified variables, only emit the strongest statement.
     * e.g. forall i: 0<=i<n ==> len > i
     * equivalent to: (0<n) ==> len >= n
     */
    if(indepOf(names, left)) {
      if(Set(LT, LTE).contains(op)) {
        // constant <= right
        extremeValue(bounds, right, maximizing = false) match {
          case Some(min) => Some(create expression(op, left, min))
          case None => None
        }
      } else {
        // constant >= right
        extremeValue(bounds, right, maximizing = true) match {
          case Some(max) => Some(create expression(op, left, max))
          case None => None
        }
      }
    } else if(indepOf(names, right)) {
      if(Set(LT, LTE).contains(op)) {
        // left <= constant
        extremeValue(bounds, left, maximizing = true) match {
          case Some(max) => Some(create expression(op, max, right))
          case None => None
        }
      } else {
        // left >= constant
        extremeValue(bounds, left, maximizing = false) match {
          case Some(min) => Some(create expression(op, min, right))
          case None => None
        }
      }
    } else {
      None
    }
  }

  def boundsNonEmpty(bounds: Map[String, (ASTNode, ASTNode)]): ASTNode = {
    bounds.values.map(bound => less(bound._1, bound._2)).reduce(and)
  }

  override def visit(expr: BindingExpression): Unit = {
    expr.binder match {
      case Binder.Forall =>
        val bounds = getBounds(expr.getDeclarations.map(_.name), expr.select) match {
          case None => super.visit(expr); return
          case Some(bounds) => bounds
        }
        val main = rewriteMain(bounds, expr.main) match {
          case None => super.visit(expr); return
          case Some(main) => main
        }
        result = create expression(Implies, boundsNonEmpty(bounds), main)
      case _ =>
        super.visit(expr)
    }
  }
}
