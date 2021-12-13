package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.features.MemberOfRange
import vct.col.newrewrite.util.Comparison
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case object SimplifyQuantifiedRelations extends RewriterBuilder

case class SimplifyQuantifiedRelations[Pre <: Generation]() extends Rewriter[Pre] {
  case object SimplifyQuantifiedRelationsOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String =
      s"At generated expression for the simplification of quantified integer relations: $message"
  }

  private implicit val o: Origin = SimplifyQuantifiedRelationsOrigin
  private def one: IntegerValue[Pre] = IntegerValue(1)

  def indepOf[G](bindings: Seq[Variable[G]], e: Expr[G]): Boolean =
    e.transSubnodes.collectFirst { case Local(ref) if bindings.contains(ref.decl) => () }.isEmpty

  case class ExtremeValue(bindings: Seq[Variable[Pre]],
                          inclusiveLowerBound: Map[Variable[Pre], ArrayBuffer[Expr[Pre]]],
                          exclusiveUpperBound: Map[Variable[Pre], ArrayBuffer[Expr[Pre]]])
  {
    def extremeValue(exprs: Seq[Expr[Pre]], maximizing: Boolean): Expr[Pre] = exprs match {
      case expr :: Nil => expr
      case left :: right :: tail =>
        Select(
          condition = if(maximizing) left > right else left < right,
          whenTrue = extremeValue(left :: tail, maximizing),
          whenFalse = extremeValue(right :: tail, maximizing),
        )
    }

    def extremeValue(expr: Expr[Pre], maximizing: Boolean): Option[Expr[Pre]] = {
      val max = (e: Expr[Pre]) => extremeValue(e, maximizing).getOrElse(return None)
      val min = (e: Expr[Pre]) => extremeValue(e, !maximizing).getOrElse(return None)

      Some(expr match {
        case v @ IntegerValue(_) => v
        case Local(Ref(v)) =>
          if(bindings.contains(v)) {
            val bounds = if(maximizing) exclusiveUpperBound(v) else inclusiveLowerBound(v)
            if(bounds.isEmpty) return None
            else if(maximizing) extremeValue(bounds.toSeq, !maximizing) - one
            else extremeValue(bounds.toSeq, !maximizing)
          } else {
            Local(v.ref)
          }
        case UMinus(inner) => min(inner)
        case Plus(left, right) => max(left) + max(right)
        case Minus(left, right) => max(left) - min(right)
        case Mult(left, right) =>
          extremeValue(Seq(
            max(left) * max(right),
            max(left) * min(right),
            min(left) * max(right),
            min(left) * min(right)
          ).distinct, maximizing)
        case Div(left, right) =>
          extremeValue(Seq(
            max(left) /: max(right),
            max(left) /: min(right),
            min(left) /: max(right),
            min(left) /: min(right)
          ).distinct, maximizing)
        case FloorDiv(left, right) =>
          extremeValue(Seq(
            max(left) / max(right),
            max(left) / min(right),
            min(left) / max(right),
            min(left) / min(right)
          ).distinct, maximizing)
        case Select(condition, whenTrue, whenFalse) => ???
        case _ => return None
      })
    }

    def maximize(expr: Expr[Pre]): Option[Expr[Pre]] = extremeValue(expr, maximizing = true)
    def minimize(expr: Expr[Pre]): Option[Expr[Pre]] = extremeValue(expr, maximizing = false)
  }

  def trySimplify(bindings: Seq[Variable[Pre]], originalBody: Expr[Pre]): Option[Expr[Pre]] = {
    if(bindings.exists(_.t != TInt())) return None

    // We split the body of the quantifier into its conditions (lhs of implies) and the body (rhs of implies)
    val (allConditions, body) = AstBuildHelpers.unfoldImplies(originalBody)
    // We filter the conditions by whether or not they contain any quantified variable
    val (globalConditions, bounds) = allConditions.partition(indepOf(bindings, _))

    // The hope is now that *every* condition in bounds is in the form `i (<|<=|==|>=|>) (bound!i)`
    val inclusiveLowerBound = bindings.map(_ -> mutable.ArrayBuffer[Expr[Pre]]()).toMap
    val exclusiveUpperBound = bindings.map(_ -> mutable.ArrayBuffer[Expr[Pre]]()).toMap

    for(bound <- bounds) {
      // First try to match a simple comparison
      Comparison.of(bound) match {
        case Some((left, comp, right)) =>
          if(comp == Comparison.NEQ) return None

          // If we have a simple comparison, one side should be independent of bindings, and the other should be
          // exactly a binding.
          if(indepOf(bindings, left)) {
            right match {
              case Local(Ref(v)) if bindings.contains(v) =>
                if(!comp.less) inclusiveLowerBound(v) += (if(comp.eq) left else left + one)
                if(!comp.greater) exclusiveUpperBound(v) += (if(comp.eq) left + one else left)
              case _ => return None
            }
          } else if(indepOf(bindings, right)) {
            left match {
              case Local(Ref(v)) if bindings.contains(v) =>
                if(!comp.less) exclusiveUpperBound(v) += (if(comp.eq) left + one else left)
                if(!comp.greater) inclusiveLowerBound(v) += (if(comp.eq) left else left + one)
              case _ => return None
            }
          } else return None
        case None => bound match {
          // If we do not have a simple comparison, we support one special case: i \in {a..b}
          case SeqMember(Local(Ref(v)), Range(from, to))
            if bindings.contains(v) && indepOf(bindings, from) && indepOf(bindings, to) =>
            inclusiveLowerBound(v) += from
            exclusiveUpperBound(v) += to
          case _ => return None
        }
      }
    }

    // We can now simplify the quantified expression, if the body is a relation (<|<=|==|>=|>) and one side is constant
    // with respect to the bindings

    val newBody = Comparison.of(body) match {
      case Some((left, baseComp, right)) =>
        val (const, comp, value) = if(indepOf(bindings, left)) {
          (left, baseComp, right)
        } else if(indepOf(bindings, right)) {
          (right, baseComp.flip, left)
        } else {
          return None
        }

        val maker = ExtremeValue(bindings, inclusiveLowerBound, exclusiveUpperBound)

        val gt = if(!comp.less) Seq(comp.make(const, maker.maximize(value).getOrElse(return None))) else Nil
        val lt = if(!comp.greater) Seq(comp.make(const, maker.minimize(value).getOrElse(return None))) else Nil

        AstBuildHelpers.foldAnd(gt ++ lt)
      case None => return None
    }

    // Success!

    Some(Implies(
      AstBuildHelpers.foldAnd(globalConditions ++ bindings.flatMap(binding =>
        inclusiveLowerBound(binding).flatMap(lower =>
          exclusiveUpperBound(binding).map(upper =>
            lower < upper
          )
        )
      )),
      newBody
    ))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case e @ Forall(bindings, _, body) =>
      trySimplify(bindings, body) match {
        case None => e.rewrite()
        case Some(e) => dispatch(e)
      }
    case other => rewriteDefault(other)
  }
}
