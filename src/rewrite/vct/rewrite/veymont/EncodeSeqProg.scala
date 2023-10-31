package vct.rewrite.veymont

import vct.col.ast.{AbstractRewriter, Declaration, SeqProg}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object EncodeSeqProg extends RewriterBuilder {
  override def key: String = "EncodeSeqProg"
  override def desc: String = "Encodes the semantics of a parallel VeyMont program"
}

case class EncodeSeqProg[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] =>
      ???
    case _ => rewriteDefault(decl)
  }
}
