package vct.col.newrewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.Rewriter

case class ForLoopToWhileLoop() extends Rewriter {
  override def dispatch(stat: Statement): Statement = stat match {
    case Loop(Block(Nil), cond, Block(Nil), contract, body) => rewriteDefault(stat)
    case Loop(init, cond, update, contract, body) =>
      implicit val o: Origin = stat.o
      Block(Seq(
        dispatch(init),
        Loop(Block(Nil), dispatch(cond), Block(Nil), dispatch(contract), Block(Seq(
          dispatch(body),
          dispatch(update),
        ))),
      ))
    case other => rewriteDefault(other)
  }
}
