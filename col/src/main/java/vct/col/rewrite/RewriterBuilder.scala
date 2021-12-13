package vct.col.rewrite

trait RewriterBuilder {
  def apply[Pre <: Generation](): Rewriter[Pre]
}
