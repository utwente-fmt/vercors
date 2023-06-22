package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.{Select, Block, Assign, Expr, IterVariable, Local, LocalDecl, Loop, LoopInvariant, IterationContract, Eval, LoopContract, PostAssignExpression, Range, RangedFor, SeqMember, Statement, Variable}
import vct.col.origin.AssignLocalOk
import vct.col.util.AstBuildHelpers.const
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

case object EncodeRangedFor extends RewriterBuilder {
  override def key: String = "encodeRangedFor"
  override def desc: String = "Encodes ranged for as a regular for loop"
}

case class EncodeRangedFor[Pre <: Generation]() extends Rewriter[Pre] {
  val bounds = ScopedStack[(RangedFor[Pre], Expr[Post])]()

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case rf @ RangedFor(iv @ IterVariable(iVar, from, to), contract, body) =>
      implicit val o = iVar.o
      val i: Local[Post] = Local(succ[Variable[Post]](iVar))(iVar.o)
      Loop(
        Block(Seq(
          LocalDecl(variables.collect(dispatch(iVar))._1.head),
          Assign(i, dispatch(from))(AssignLocalOk)
        ))(iVar.o),
        { implicit val o = iv.o; SeqMember(i, Range(dispatch(from), dispatch(to))) },
        Eval(PostAssignExpression(i, i + const(1))(AssignLocalOk)),
        bounds.having((rf,
          { implicit val o = iv.o;
            Select(dispatch(from) < dispatch(to),
              SeqMember(i, Range(dispatch(from), dispatch(to) + const(1))),
              i === dispatch(from))
          }))(dispatch(contract)),
        dispatch(body)
      )(rf.o)
    case stat => rewriteDefault(stat)
  }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] = (bounds.topOption, contract) match {
    case (Some((rf, iBounds)), l: LoopInvariant[Pre]) =>
      l.rewrite(invariant = (iBounds &* dispatch(l.invariant))(rf.o))
    case (Some((rf, iBounds)), ic: IterationContract[Pre]) =>
      ic.rewrite(context_everywhere = (iBounds &* dispatch(ic.context_everywhere))(rf.o))
    case (None, c) => rewriteDefault(c)
  }
}
