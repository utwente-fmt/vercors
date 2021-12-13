package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.Unreachable

case object Disambiguate extends RewriterBuilder

case class Disambiguate[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case op @ AmbiguousMult(left, right) =>
        if(op.isProcessOp) ProcessSeq(dispatch(left), dispatch(right))
        else Mult(dispatch(left), dispatch(right))
      case op @ AmbiguousPlus(left, right) =>
        if(op.isProcessOp) ProcessChoice(dispatch(left), dispatch(right))
        else if(op.isPointerOp) PointerAdd(dispatch(left), dispatch(right))(op.blame)
        else if(op.isSeqOp) Concat(dispatch(left), dispatch(right))
        else if(op.isSetOp) ???
        else Plus(dispatch(left), dispatch(right))
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
      case other => rewriteDefault(other)
    }
  }
}
