package vct.col.rewrite

import vct.col.ast.AbstractRewriter

/**
 * Defines how to construct a [[Rewriter]] for the transformation stage.
 *
 * If the rewriter needs arguments, refer to [[RewriterBuilderArg]]
 *
 * `key` is a short (user-facing) camelCase identifier for the rewriter.
 *
 * `desc` is a one-sentence description of the rewriter.
 *
 * `apply` defines how to construct the rewriter. Note that for case classes the companion object already defines
 * `apply` implicitly:
 *
 * {{{
 * case object Test extends RewriterBuilder {
 *   def key: String = ???
 *   def desc: String = ???
 * }
 *
 * case class Test[Pre <: Generation]() extends Rewriter[Pre]
 * }}}
 */
trait RewriterBuilder {
  def apply[Pre <: Generation](): AbstractRewriter[Pre, _ <: Generation]
  def key: String
  def desc: String
}
