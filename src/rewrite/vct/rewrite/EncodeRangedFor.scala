package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.{Assign, Block, Eval, Expr, IterVariable, IterationContract, Local, LocalDecl, Loop, LoopContract, LoopInvariant, PostAssignExpression, Range, RangedFor, Select, SeqMember, Statement, TInt, Variable}
import vct.col.origin.{AssignLocalOk, Origin}
import vct.col.util.AstBuildHelpers.const
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

case object EncodeRangedFor extends RewriterBuilder {
  override def key: String = "encodeRangedFor"
  override def desc: String = "Encodes ranged for as a regular for loop"
}

case class EncodeRangedFor[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeRangedFor._

  val bounds = ScopedStack[(RangedFor[Pre], Expr[Post])]()

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case rf @ RangedFor(iv @ IterVariable(iVar, fromExpr, toExpr), contract, body) =>
      implicit val o = iVar.o
      val i: Local[Post] = Local(succ[Variable[Post]](iVar))(iVar.o)

      val fromVar = new Variable[Post](TInt()(fromExpr.o))(fromExpr.o.where(name = "from"))
      val from = Local(fromVar.ref[Variable[Post]])(fromExpr.o.where(name = "from"))

      val toVar = new Variable[Post](TInt()(toExpr.o))(toExpr.o.where(name = "to"))
      val to = Local(toVar.ref[Variable[Post]])(toExpr.o.where(name = "to"))

      Loop(
        Block(Seq(
          LocalDecl(fromVar)(fromExpr.o),
          LocalDecl(toVar)(toExpr.o),
          LocalDecl(variables.collect(dispatch(iVar))._1.head),
          Assign(from, dispatch(fromExpr))(AssignLocalOk),
          Assign(to, dispatch(toExpr))(AssignLocalOk),
          Assign(i, from)(AssignLocalOk)
        ))(iVar.o),
        { implicit val o = iv.o; SeqMember(i, Range(from, to)) },
        Eval(PostAssignExpression(i, i + const(1))(AssignLocalOk)),
        bounds.having((rf,
          { implicit val o = iv.o;
            Select(from < to,
              SeqMember(i, Range(from, to + const(1))),
              i === from)
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
