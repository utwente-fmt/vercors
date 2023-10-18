package vct.col.rewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object ForLoopToWhileLoop extends RewriterBuilder {
  override def key: String = "forLoop"
  override def desc: String =
    "Translate for loops into while loops by putting initialization and the update before and in the loop."
}

case class ForLoopToWhileLoop[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case Loop(Block(Nil), cond, Block(Nil), contract, body) =>
        rewriteDefault(stat)
      case loop @ Loop(init, cond, update, contract, body) =>
        implicit val o: Origin = stat.o
        Block(Seq(
          dispatch(init),
          Loop(
            Block(Nil),
            dispatch(cond),
            Block(Nil),
            dispatch(contract),
            Block(Seq(dispatch(body), dispatch(update))),
          ),
        ))
      case other => rewriteDefault(other)
    }
}
