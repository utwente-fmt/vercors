package vct.col.newrewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.result.VerificationError.Unreachable

case object TagMinimizationTargets extends RewriterBuilderArg[(Seq[String], Seq[String])] {
  override def key: String = "tagMinimizationTargets"
  override def desc: String = "Tags nodes that are targets for minimization"

  case class FocusTarget(o: Origin) extends Origin {
    override def preferredName: String = o.preferredName
    override def shortPosition: String = o.shortPosition
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
  }

  case class IgnoreTarget(o: Origin) extends Origin {
    override def preferredName: String = o.preferredName
    override def shortPosition: String = o.shortPosition
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
  }
}

// Maybe we should factor out the "marker origin" pattern that is just transparent, like the two above?

// For now I have the use case of command line flags in mind, but maybe we want to support it on the input level as well?
// Or maybe only on the input level? With focus/ignore modifiers?

// For now, assumes unique method names

case class TagMinimizationTargets[Pre <: Generation](rawNames: (Seq[String], Seq[String])) extends Rewriter[Pre] {
  import TagMinimizationTargets.FocusTarget
  import TagMinimizationTargets.IgnoreTarget

  lazy val rawFocusNames: Seq[String] = rawNames._1
  lazy val rawIgnoreNames: Seq[String] = rawNames._2

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case declaration: InstanceMethod[Pre] if rawFocusNames.contains(declaration.o.preferredName) =>
      declaration.rewrite(o = FocusTarget(declaration.o)).succeedDefault(declaration)
    case declaration: InstanceMethod[Pre] if rawIgnoreNames.contains(declaration.o.preferredName) =>
      declaration.rewrite(o = IgnoreTarget(declaration.o)).succeedDefault(declaration)
    case _ => rewriteDefault(decl)
  }
}