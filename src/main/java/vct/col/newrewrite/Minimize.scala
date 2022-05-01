package vct.col.newrewrite

import vct.col.ast.{ADTDeclaration, Applicable, CDeclaration, CParam, ClassDeclaration, Declaration, GlobalDeclaration, InstanceFunction, InstanceMethod, JavaLocalDeclaration, LabelDecl, ModelDeclaration, ParBlockDecl, ParInvariantDecl, Procedure, Program, SendDecl, Variable}
import vct.col.newrewrite.Minimize.{AbstractedFunctionOrigin, computeFocus, getUsedDecls, makeOthersAbstract, removeUnused}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast._
import RewriteHelpers._
import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.result.VerificationError.{SystemError, Unreachable}

case object Minimize extends RewriterBuilder {
  override def key: String = "minimize"
  override def desc: String = "Minimize AST based on indicated minimization targets"

  case class AbstractedFunctionOrigin[G](f: Function[G]) extends Origin {
    override def preferredName: String = "result"
    override def context: String = f.o.context
    override def inlineContext: String = f.o.inlineContext
    override def shortPosition: String = f.o.shortPosition
  }

  case class GenericMinimizeTarget(`class`: Seq[String], name: String, mode: MinimizeMode)

  sealed abstract class MinimizeMode()
  object MinimizeMode {
    final case object Focus extends MinimizeMode
    final case object Ignore extends MinimizeMode
  }

  def isFocus[G](decl: Declaration[G]) = decl.o.isInstanceOf[TagMinimizationTargets.FocusTarget]

  def computeFocus[G](p: Program[G]): Seq[Declaration[G]] =
    p.transSubnodes.collect({case decl: Declaration[G] => decl}).filter(isFocus)

  def makeOthersAbstract[Pre <: Generation](p: Program[Pre], doNotChange: Seq[Declaration[Pre]]): Program[Rewritten[Pre]] =
    AbstractMaker(doNotChange).dispatch(p)

  def removeUnused[Pre <: Generation](p: Program[Pre], used: Seq[Declaration[Pre]]): (Program[Rewritten[Pre]], Int) = {
    val ru = RemoveUnused(used)
    (ru.dispatch(p), ru.numDropped)
  }

  // Get all predicate usages, method usages, field usages
  def getUsedDecls[G](p: Program[G]): Seq[Declaration[G]] = {
    p.transSubnodes.collect {
      case in: InvokingNode[G] => in.ref.decl
      case app: Apply[G] => app.ref.decl
      case Deref(_, r) => r.decl
    }
  }
}

case class AbstractMaker[Pre <: Generation](focusTargets: Seq[Declaration[Pre]]) extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = {
    val b = !focusTargets.contains(decl)
    decl match {
      case p: Procedure[Pre] if b => p.rewrite(body = None).succeedDefault(p)
      case f: Function[Pre] if b && f.body.isDefined =>
        implicit val o = AbstractedFunctionOrigin(f)
        withResult((result: Result[Post]) => {
          val resultEqualsBody: Eq[Post] = result === dispatch(f.body.get)
          val ensures = SplitAccountedPredicate(dispatch(f.contract.ensures), UnitAccountedPredicate(resultEqualsBody))
          f.rewrite(contract = f.contract.rewrite(ensures = ensures), body = None).succeedDefault(f)
        })
      case d => super.dispatch(d)
    }
  }
}

case class RemoveUnused[Pre <: Generation](used: Seq[Declaration[Pre]]) extends Rewriter[Pre] with LazyLogging {
  var numDropped = 0

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case _: Procedure[Pre] | _: Function[Pre] | _: Predicate[Pre] | _: Field[Pre] if !used.contains(decl) =>
        logger.debug(s"Dropping: ${decl.o.preferredName}, ${decl.o.getClass.getSimpleName}")
        decl.drop()
        numDropped += 1
      case _ => rewriteDefault(decl)
    }
  }
}

case class Minimize[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  override def dispatch(p: Program[Pre]): Program[Post] = {
    val focusTargets = computeFocus(p)
    var program: Program[Post] = makeOthersAbstract(p, focusTargets)
    var changeHappened = true
    var totalDropped = 0

    while (changeHappened) {
      val (programNew, numDropped) = removeUnused(program, getUsedDecls(program) ++ computeFocus(program))
      program = programNew.asInstanceOf[Program[Post]]
      changeHappened = numDropped > 0
      totalDropped += numDropped
    }

    logger.debug(s"Number of declarations dropped: $totalDropped")

    program
  }
}
