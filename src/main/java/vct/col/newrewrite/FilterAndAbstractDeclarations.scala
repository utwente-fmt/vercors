package vct.col.newrewrite

import vct.col.ast.{ADTDeclaration, Applicable, CDeclaration, CParam, ClassDeclaration, Declaration, GlobalDeclaration, InstanceFunction, InstanceMethod, JavaLocalDeclaration, LabelDecl, ModelDeclaration, ParBlockDecl, ParInvariantDecl, Procedure, Program, SendDecl, Variable}
import vct.col.newrewrite.FilterAndAbstractDeclarations.{AbstractedFunctionOrigin, getUsedDecls, makeOthersAbstract, removeUnused}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast._
import RewriteHelpers._
import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.result.VerificationError.{SystemError, Unreachable}

import scala.collection.mutable

case object FilterAndAbstractDeclarations extends RewriterBuilder {
  override def key: String = "filterAndAbstractDeclarations"
  override def desc: String = "Filter and abstract declarations based on indicated minimization targets"

  case class AbstractedFunctionOrigin[G](f: Function[G]) extends Origin {
    override def preferredName: String = "result"
    override def context: String = f.o.context
    override def inlineContext: String = f.o.inlineContext
    override def shortPosition: String = f.o.shortPosition
  }

  def makeOthersAbstract[Pre <: Generation](p: Program[Pre], focusTargets: Seq[Declaration[Pre]], ignoreTargets: Seq[Declaration[Pre]]): Program[Rewritten[Pre]] =
    AbstractMaker(focusTargets, ignoreTargets).dispatch(p)

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

case class AbstractMaker[Pre <: Generation](focusTargets: Seq[Declaration[Pre]], ignoreTargets: Seq[Declaration[Pre]]) extends Rewriter[Pre] {
  // If there are no focus targets, only ignored decls are abstracted
  // If there are focus targets, abstract everything not-focused
  // It is assumed there is no overlap between focus & ignore sets
  def makeAbstract(decl: Declaration[Pre]): Boolean =
    if (focusTargets.isEmpty) { ignoreTargets.contains(decl) }
    else { !focusTargets.contains(decl) }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] if makeAbstract(p) => p.rewrite(body = None).succeedDefault(p)
      case f: Function[Pre] if makeAbstract(f) && f.body.isDefined =>
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

case class FilterAndAbstractDeclarations[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  def getIgnored[G](n: Program[G]): Seq[ContractApplicable[G]] = n.transSubnodes.collect({case ca: ContractApplicable[G] if ca.ignore => ca})
  def getFocused[G](n: Program[G]): Seq[ContractApplicable[G]] = n.transSubnodes.collect({case ca: ContractApplicable[G] if ca.focus => ca})

  override def dispatch(p: Program[Pre]): Program[Post] = {
    val focusTargets = getFocused(p)
    val ignoreTargets = getIgnored(p)

    if (focusTargets.isEmpty && ignoreTargets.isEmpty) {
      p.rewrite()
    } else if (focusTargets.isEmpty) {
      // Ignored declarations are kept around if other declarations need them, and dropped if possible
      // The difference between only abstracting and also dropping is (= should not) not observable,
      // but it does make the final output textually smaller, which is nice for getting to a minimal working example quickly.
      val program: Program[Pre] = makeOthersAbstract(p, focusTargets, ignoreTargets).asInstanceOf[Program[Pre]]
      val allDecls: Set[Declaration[Pre]] = program.transSubnodes.collect({ case d: Declaration[Pre] => d }).toSet
      val keep: Set[Declaration[Pre]] = (allDecls -- getIgnored(program).toSet) ++ getUsedDecls[Pre](program).toSet
      removeUnused(program, keep.toSeq)._1
    } else {
      var program: Program[Post] = makeOthersAbstract(p, focusTargets, ignoreTargets)
      var dropped: Seq[Declaration[Post]] = Nil

      do {
        val (programNew, droppedNew) = removeUnused(program, getUsedDecls(program) ++ getFocused(program))
        program = programNew.asInstanceOf[Program[Post]]
        dropped = droppedNew
      } while (dropped.nonEmpty)

      program
    }
  }
}
