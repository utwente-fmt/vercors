package vct.col.rewrite

import vct.col.ast.AbstractRewriter

trait RewriterBuilderArg[T] {
  def apply[Pre <: Generation](x: T): AbstractRewriter[Pre, _ <: Generation]
  def key: String
  def desc: String

  def withArg(x: T): RewriterBuilder =
    new RewriterBuilder {
      override def apply[Pre <: Generation](): AbstractRewriter[Pre, _ <: Generation] = RewriterBuilderArg.this.apply(x)
      override def key: String = RewriterBuilderArg.this.key
      override def desc: String = RewriterBuilderArg.this.desc
    }
}
