package vct.col.newrewrite

import vct.col.ast.{Branch, Statement}
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

case class BranchToIfElse() extends Rewriter {
  override def dispatch(stat: Statement): Statement = stat match {
    case Branch(Seq((cond, whenTrue), (`tt`, whenFalse))) =>
      Branch(Seq((cond, whenTrue), (tt, whenFalse)))(stat.o)
    case Branch((cond, whenTrue) +: tail) =>
      Branch(Seq(
        (cond, whenTrue),
        (tt, dispatch(Branch(tail)(stat.o)))
      ))(stat.o)
    case other => rewriteDefault(other)
  }
}
