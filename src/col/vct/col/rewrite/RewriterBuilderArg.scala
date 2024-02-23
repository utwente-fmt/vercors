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

/**
 * As RewriterBuilderArg, but with 2 parameters.
 * @tparam T
 * @tparam U
 */
trait RewriterBuilderArg2[T, U] {
  def apply[Pre <: Generation](t: T, u: U): AbstractRewriter[Pre, _ <: Generation]
  def key: String
  def desc: String

  def withArg(t: T, u: U): RewriterBuilder =
    new RewriterBuilder {
      override def apply[Pre <: Generation](): AbstractRewriter[Pre, _ <: Generation] = RewriterBuilderArg2.this.apply(t, u)
      override def key: String = RewriterBuilderArg2.this.key
      override def desc: String = RewriterBuilderArg2.this.desc
    }
}
