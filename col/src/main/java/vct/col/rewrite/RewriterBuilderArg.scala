package vct.col.rewrite

import vct.col.ast.AbstractRewriter

/**
 * Defines how to construct a [[Rewriter]] for the transformation stage, when the rewriter takes an argument.
 *
 * Similarly to [[RewriterBuilder]], for case classes this shape of `apply` is implicitly defined:
 *
 * {{{
 * case object Test extends RewriterBuilderArg[Int] {
 *   def key: String = ???
 *   def desc: String = ???
 * }
 *
 * case class Test[Pre <: Generation](x: Int) extends Rewriter[Pre]
 * }}}
 *
 * @tparam T The type of the argument the rewriter takes.
 */
trait RewriterBuilderArg[T] {
  def apply[Pre <: Generation](x: T): AbstractRewriter[Pre, _ <: Generation]
  def key: String
  def desc: String

  /**
   * Constructs a [[RewriterBuilder]] with the argument to the rewriter pinned.
   * @param x The pinned parameter.
   */
  def withArg(x: T): RewriterBuilder =
    new RewriterBuilder {
      override def apply[Pre <: Generation](): AbstractRewriter[Pre, _ <: Generation] = RewriterBuilderArg.this.apply(x)
      override def key: String = RewriterBuilderArg.this.key
      override def desc: String = RewriterBuilderArg.this.desc
    }
}
