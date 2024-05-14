package vct.rewrite.lang

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import RewriteHelpers._

case object NoSupportSelfLoop extends RewriterBuilder {
  override def key: String = "removeSupportSelfLoop"
  override def desc: String =
    "Remove the self-loop in java.lang.Object extends java.lang.Object."
}

case class NoSupportSelfLoop[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case cls: ByReferenceClass[Pre] =>
        globalDeclarations.succeed(
          cls,
          cls.rewrite(supports =
            cls.supports.filter(_.asClass.get.cls.decl != cls)
              .map(_.rewriteDefault())
          ),
        )
      case cls: ByValueClass[Pre] =>
        globalDeclarations.succeed(
          cls,
          cls.rewrite(supports =
            cls.supports.filter(_.asClass.get.cls.decl != cls)
              .map(_.rewriteDefault())
          ),
        )
      case other => rewriteDefault(other)
    }
}
