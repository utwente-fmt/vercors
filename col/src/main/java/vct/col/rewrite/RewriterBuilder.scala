package vct.col.rewrite

import vct.col.ast.AbstractRewriter

trait RewriterBuilder {
  def apply[Pre <: Generation](): AbstractRewriter[Pre, _ <: Generation]
}
