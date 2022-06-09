package vct.col.newrewrite

import vct.col.ast.{ADTDeclaration, Applicable, CDeclaration, CParam, ClassDeclaration, Declaration, GlobalDeclaration, InstanceFunction, InstanceMethod, JavaLocalDeclaration, LabelDecl, ModelDeclaration, ParBlockDecl, ParInvariantDecl, Procedure, Program, SendDecl, Variable}
import vct.col.newrewrite.Minimize.{AbstractedFunctionOrigin, computeFocus, getUsedDecls, makeOthersAbstract, removeUnused}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast._
import RewriteHelpers._
import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.options.MinimizeMode
import vct.result.VerificationError.{SystemError, Unreachable}

import scala.collection.mutable

case object Minimize extends RewriterBuilder {
  override def key: String = "minimize"
  override def desc: String = "Minimize AST based on indicated minimization targets"

  case class AbstractedFunctionOrigin[G](f: Function[G]) extends Origin {
    override def preferredName: String = "result"
    override def context: String = f.o.context
    override def inlineContext: String = f.o.inlineContext
    override def shortPosition: String = f.o.shortPosition
  }

  // Currently the minimization system piggybacks on the origin system.
  // To improve: the minimization targets could be a first-class property of the Program type
  // Similar to how the rootObject is a property of Program
  case class MinimizeOrigin(o: Origin, mode: MinimizeMode) extends Origin {
    override def preferredName: String = o.preferredName
    override def shortPosition: String = o.shortPosition
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
  }

  def isFocus[G](decl: Declaration[G]) = decl.o match {
    case MinimizeOrigin(_, MinimizeMode.Focus) => true
    case _ => false
  }

  def computeFocus[G](p: Program[G]): Seq[Declaration[G]] =
    p.transSubnodes.collect({case decl: Declaration[G] => decl}).filter(isFocus)

  def makeOthersAbstract[Pre <: Generation](p: Program[Pre], doNotChange: Seq[Declaration[Pre]]): Program[Rewritten[Pre]] =
    AbstractMaker(doNotChange).dispatch(p)

  def removeUnused[Pre <: Generation](p: Program[Pre], used: Seq[Declaration[Pre]]): (Program[Rewritten[Pre]], Seq[Declaration[Pre]]) = {
    val ru = RemoveUnused(used)
    (ru.dispatch(p), ru.dropped.toSeq)
  }

  // Get all predicate usages, method usages, field usages, adt usages, adt function usages
  def getUsedDecls[G <: Generation](p: Program[G]): Seq[Declaration[G]] = {
    val collected: ScopedStack[mutable.Set[Declaration[G]]] = ScopedStack()

    // TODO (RR): Should this be a visitor? But we don't generate that
    case class Collector() extends Rewriter[G] {
      override def dispatch(expr: Expr[G]): Expr[Rewritten[G]] = {
        expr match {
          case pi: ProcedureInvocation[G] =>
            collected.top.add(pi.ref.decl)
          case fi: FunctionInvocation[G] =>
            collected.top.add(fi.ref.decl)
          case pa: PredicateApply[G] =>
            collected.top.add(pa.ref.decl)
          case afi: ADTFunctionInvocation[G] =>
            collected.top.add(afi.ref.decl)
          case Deref(_, r) =>
            collected.top.add(r.decl)
          case _ =>
        }
        super.dispatch(expr)
      }

      override def dispatch(t: Type[G]): Type[Rewritten[G]] = {
        t match {
          case TAxiomatic(r, _) => collected.top.add(r.decl)
          case _ =>
        }
        super.dispatch(t)
      }

      override def dispatch(s: Statement[G]): Statement[Rewritten[G]] = {
        s match {
          case ip: InvokeProcedure[G] => collected.top.add(ip.ref.decl)
          case _ =>
        }
        super.dispatch(s)
      }

      override def dispatch(decl: Declaration[G]): Unit = decl match {
        case _: Procedure[G] | _: Predicate[G] | _: Function[G] | _: AxiomaticDataType[G] =>
          val `collected'` = collected.saving(mutable.Set()) {
            super.dispatch(decl)
            collected.top.remove(decl)
            decl match {
              case a: AxiomaticDataType[G] => a.functions.foreach(f => collected.top.remove(f))
              case _ =>
            }
          }
          collected.top.addAll(`collected'`)
        case d =>
          super.dispatch(d)
      }
    }

    collected.saving(mutable.Set()) {
      Collector().dispatch(p)
    }.toSeq
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
  var dropped: mutable.Set[Declaration[Pre]] = mutable.Set()

  def adtIsUsed(a: AxiomaticDataType[Pre]): Boolean =
    used.contains(a) || a.functions.exists(used.contains(_))

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case _: Procedure[Pre] | _: Function[Pre] | _: Predicate[Pre] | _: Field[Pre] if !used.contains(decl) =>
        logger.debug(s"Dropping: ${decl.o.preferredName}, ${decl.o.getClass.getSimpleName}")
        decl.drop()
        dropped.add(decl)
      case a: AxiomaticDataType[Pre] if !adtIsUsed(a) =>
        logger.debug(s"Dropping: ${a.o.preferredName}, ${a.o.getClass.getSimpleName}, and all its adt functions")
        a.drop()
        a.decls.foreach(_.drop())
        dropped.add(decl)
      case _ => rewriteDefault(decl)
    }
  }
}

case class Minimize[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  override def dispatch(p: Program[Pre]): Program[Post] = {
    val focusTargets = computeFocus(p)
    var program: Program[Post] = makeOthersAbstract(p, focusTargets)
    var dropped: Seq[Declaration[Post]] = Nil

    do {
      val (programNew, droppedNew) = removeUnused(program, getUsedDecls(program) ++ computeFocus(program))
      program = programNew.asInstanceOf[Program[Post]]
      dropped = droppedNew
    } while (dropped.nonEmpty)

    program
  }
}
