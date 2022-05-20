package vct.col.newrewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object EncodeForkJoin extends RewriterBuilder {
  override def key: String = "forkJoin"
  override def desc: String = "Encode fork and join statements with the contract of the run method it refers to."
}

case class EncodeForkJoin[Pre <: Generation]() extends Rewriter[Pre] {

}
