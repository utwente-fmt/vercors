package vct.col.rewrite

import vct.col.ast.{Block, BooleanValue, Branch, Statement}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

case object BranchToIfElse extends RewriterBuilder {
  override def key: String = "branchToIfElse"
  override def desc: String = "Translate a chain of if/elseif/else to strictly if/else."
}

case class BranchToIfElse[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Branch(Nil) => Block(Nil)(stat.o)
    case Branch(Seq((cond, whenTrue), (BooleanValue(true), whenFalse))) =>
      Branch(Seq((dispatch(cond), dispatch(whenTrue)), (tt[Post], dispatch(whenFalse))))(stat.o)
    case Branch((cond, whenTrue) +: tail) =>
      Branch(Seq(
        (dispatch(cond), dispatch(whenTrue)),
        (tt[Post], dispatch(Branch[Pre](tail)(stat.o)))
      ))(stat.o)
    case other => rewriteDefault(other)
  }
}
