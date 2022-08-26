package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.newrewrite.Disambiguate.NotALocation
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.{Unreachable, UserError}
import viper.silicon.state.terms.SetDifference

case object Disambiguate extends RewriterBuilder {

  case class NotALocation(expr: Expr[_]) extends UserError {
    override def code: String = "notALocation"

    override def text: String = expr.o.messageInContext("This expression is not a heap location")
  }

  override def key: String = "disambiguate"
  override def desc: String = "Translate ambiguous operators into concrete operators."
}

case class Disambiguate[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case op @ AmbiguousMult(left, right) =>
        if(op.isProcessOp) ProcessSeq(dispatch(left), dispatch(right))
        else if(op.isSetOp) SetIntersection(dispatch(left), dispatch(right))
        else if(op.isBagOp) BagLargestCommon(dispatch(left), dispatch(right))
        else Mult(dispatch(left), dispatch(right))
      case op @ AmbiguousPlus(left, right) =>
        if(op.isProcessOp) ProcessChoice(dispatch(left), dispatch(right))
        else if(op.isPointerOp) PointerAdd(dispatch(left), dispatch(right))(op.blame)
        else if(op.isSeqOp) Concat(dispatch(left), dispatch(right))
        else if(op.isSetOp) SetUnion(dispatch(left), dispatch(right))
        else if(op.isBagOp) BagAdd(dispatch(left), dispatch(right))
        else Plus(dispatch(left), dispatch(right))
      case op @ AmbiguousMinus(left, right) =>
        if(op.isSetOp) SetMinus(dispatch(left), dispatch(right))
        else if(op.isBagOp) BagMinus(dispatch(left), dispatch(right))
        else Minus(dispatch(left), dispatch(right))
      case op @ AmbiguousOr(left, right) =>
        if(op.isProcessOp) ProcessPar(dispatch(left), dispatch(right))
        else Or(dispatch(left), dispatch(right))
      case op: BitOp[Pre] =>
        val cons = if(op.isBoolOp) op match {
          case _: AmbiguousComputationalOr[Pre] => Or[Post](_, _)
          case _: AmbiguousComputationalXor[Pre] => Neq[Post](_, _)
          case _: AmbiguousComputationalAnd[Pre] => And[Post](_, _)
        } else op match {
          case _: AmbiguousComputationalOr[Pre] => ComputationalOr[Post](_, _)
          case _: AmbiguousComputationalXor[Pre] => ComputationalXor[Post](_, _)
          case _: AmbiguousComputationalAnd[Pre] => ComputationalAnd[Post](_, _)
        }

        cons(dispatch(op.left), dispatch(op.right))
      case op @ AmbiguousSubscript(collection, index) =>
        if(op.isPointerOp) PointerSubscript(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isMapOp) MapGet(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isArrayOp) ArraySubscript(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isSeqOp) SeqSubscript(dispatch(collection), dispatch(index))(op.blame)
        else throw Unreachable("AmbiguousSubscript must subscript a pointer, map, array, or seq because of the type check.")
      case op @ AmbiguousMember(x, xs) =>
        if(op.isMapOp) MapMember(dispatch(x), dispatch(xs))
        else if(op.isSetOp) SetMember(dispatch(x), dispatch(xs))
        else if(op.isBagOp) BagMemberCount(dispatch(x), dispatch(xs))
        else if(op.isSeqOp) SeqMember(dispatch(x), dispatch(xs))
        else throw Unreachable("AmbiguousMember must query a map, set, bag, or seq because of the type check.")
      case cmp: AmbiguousOrderOp[Pre] =>
        if(cmp.isBagOp) cmp match {
          case AmbiguousGreater(left, right) => SubBag(dispatch(right), dispatch(left))
          case AmbiguousLess(left, right) => SubBag(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => SubBagEq(dispatch(right), dispatch(left))
          case AmbiguousLessEq(left, right) => SubBagEq(dispatch(left), dispatch(right))
        } else if(cmp.isSetOp) cmp match {
          case AmbiguousGreater(left, right) => SubSet(dispatch(right), dispatch(left))
          case AmbiguousLess(left, right) => SubSet(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => SubSetEq(dispatch(right), dispatch(left))
          case AmbiguousLessEq(left, right) => SubSetEq(dispatch(left), dispatch(right))
        } else cmp match {
          case AmbiguousGreater(left, right) => Greater(dispatch(left), dispatch(right))
          case AmbiguousLess(left, right) => Less(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => GreaterEq(dispatch(left), dispatch(right))
          case AmbiguousLessEq(left, right) => LessEq(dispatch(left), dispatch(right))
        }
      case other => rewriteDefault(other)
    }
  }


  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case AmbiguousLocation(expr) => {
        expr match {
          case Deref(obj, ref) =>
            FieldLocation(dispatch(obj), succ(ref))
          case ModelDeref(obj, ref) =>
            ModelLocation(dispatch(obj), succ(ref))
          case SilverDeref(obj, ref) =>
            SilverFieldLocation(dispatch(obj), succ(ref))
          case ArraySubscript(arr, index) =>
            ArrayLocation(dispatch(arr), dispatch(index))
          case expr if expr.t.asPointer.isDefined =>
            PointerLocation(dispatch(expr))
          case PredicateApply(ref, args, WritePerm()) =>
            PredicateLocation(succ(ref), (args.map(dispatch)))
          case InstancePredicateApply(obj, ref, args, WritePerm()) =>
            InstancePredicateLocation(succ(ref), dispatch(obj), args.map(dispatch))
          case default =>
            throw NotALocation(default)
        }
    }
      case other => rewriteDefault(other)
    }
  }
}
