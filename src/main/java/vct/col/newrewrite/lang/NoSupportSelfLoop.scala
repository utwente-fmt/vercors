package vct.col.newrewrite.lang

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import RewriteHelpers._

case object NoSupportSelfLoop extends RewriterBuilder

case class NoSupportSelfLoop[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      cls.rewrite(supports = cls.supports.filter(_.decl != cls).map(succ[Class[Post]])).succeedDefault(cls)
    case other => rewriteDefault(other)
  }
}
