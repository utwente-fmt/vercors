package vct.col.rewrite.lang

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import RewriteHelpers._

case object NoSupportSelfLoop extends RewriterBuilder {
  override def key: String = "removeSupportSelfLoop"
  override def desc: String = "Remove the self-loop in java.lang.Object extends java.lang.Object."
}

case class NoSupportSelfLoop[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      globalDeclarations.succeed(cls, cls.rewrite(supports = cls.supports.map(_.decl).filter(_ != cls).map(succ[Class[Post]])))
    case other => rewriteDefault(other)
  }
}
