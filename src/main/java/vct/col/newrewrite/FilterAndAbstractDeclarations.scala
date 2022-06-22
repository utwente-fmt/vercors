package vct.col.newrewrite

import vct.col.ast.{ADTDeclaration, Applicable, CDeclaration, CParam, ClassDeclaration, Declaration, GlobalDeclaration, InstanceFunction, InstanceMethod, JavaLocalDeclaration, LabelDecl, ModelDeclaration, ParBlockDecl, ParInvariantDecl, Procedure, Program, SendDecl, Variable}
import vct.col.newrewrite.FilterAndAbstractDeclarations.{AbstractedFunctionOrigin, filterAndAbstract, getUsedDecls}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast._
import RewriteHelpers._
import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.newrewrite.lang.LangJavaToCol.{JavaAnnotationMethodOrigin, JavaConstructorOrigin, JavaInstanceFunctionOrigin, JavaMethodOrigin}
import vct.col.newrewrite.lang.LangPVLToCol.PVLSourceNameOrigin
import vct.col.origin.{DiagnosticOrigin, Origin, SourceNameOrigin}
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

  def filterAndAbstract[G <: Generation](p: Program[G], focused: Seq[ContractApplicable[G]], ignored: Seq[ContractApplicable[G]]): (Program[Rewritten[G]], Seq[Declaration[_]]) =
    if (focused.isEmpty && ignored.isEmpty) {
      (new Rewriter[G]().dispatch(p), Nil)
    } else if (focused.isEmpty) {
      // Ignored declarations are kept around if other declarations need them, and dropped if possible
      // The difference between only abstracting and also dropping is (= should not) not observable,
      // but it does make the final output textually smaller, which is nice for getting to a minimal working example quickly.
      val am = AbstractMaker[G](focused, ignored)
      val program1 = am.dispatch(p)
      val ignored1 = ignored.map(am.succ[Declaration[Rewritten[G]]](_).decl).toSet
      val allDecls = program1.transSubnodes.collect({ case d: Declaration[Rewritten[G]] => d }).toSet
      val used = (allDecls -- ignored1) ++ getUsedDecls(program1).toSet
      val ru = RemoveUnused[Rewritten[G]](used.toSeq)
      (ru.dispatch(program1).asInstanceOf[Program[Rewritten[G]]], ru.dropped.toSeq)
    } else {
      val am = AbstractMaker(focused, ignored)
      var program1: Program[Rewritten[G]] = am.dispatch(p)
      var focused1: Seq[Declaration[Rewritten[G]]] = focused.map(am.succ[Declaration[Rewritten[G]]](_).decl)
      var dropped: Seq[Declaration[Rewritten[G]]] = Nil
      var totalDropped: Seq[Declaration[_]] = Nil

      do {
        val ru = RemoveUnused[Rewritten[G]](getUsedDecls(program1) ++ focused1)
        val program2 = ru.dispatch(program1)
        program1 = program2.asInstanceOf[Program[Rewritten[G]]]
        focused1 = focused1.map(ru.succ[Declaration[Rewritten[Rewritten[G]]]](_).decl).asInstanceOf[Seq[Declaration[Rewritten[G]]]]
        dropped = ru.dropped.toSeq
        totalDropped ++= dropped
      } while (dropped.nonEmpty)

      (program1, totalDropped)
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
          val ensures = SplitAccountedPredicate(UnitAccountedPredicate(resultEqualsBody), dispatch(f.contract.ensures))
          f.rewrite(contract = f.contract.rewrite(ensures = ensures), body = None).succeedDefault(f)
        })
      case d => super.dispatch(d)
    }
  }
}

case class RemoveUnused[Pre <: Generation](used: Seq[Declaration[Pre]]) extends Rewriter[Pre] with LazyLogging {
  var dropped: mutable.Set[Declaration[Pre]] = mutable.Set()
  var notified: Int = 0

  def adtIsUsed(a: AxiomaticDataType[Pre]): Boolean =
    used.contains(a) || a.functions.exists(used.contains(_))

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case _: Procedure[Pre] | _: Function[Pre] | _: Predicate[Pre] | _: Field[Pre] if !used.contains(decl) =>
        decl.drop()
        dropped.add(decl)
      case a: AxiomaticDataType[Pre] if !adtIsUsed(a) =>
        a.drop()
        a.decls.foreach({ d => d.drop(); dropped.add(d) })
        dropped.add(decl)
      case _ => rewriteDefault(decl)
    }
  }
}

case class FilterAndAbstractDeclarations[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  def getFocused[G](n: Program[G]): Seq[ContractApplicable[G]] = n.transSubnodes.collect({case ca: ContractApplicable[G] if ca.focus => ca})
  def getIgnored[G](n: Program[G]): Seq[ContractApplicable[G]] = n.transSubnodes.collect({case ca: ContractApplicable[G] if ca.ignore => ca})

  def getNiceName[G](d: Declaration[G]): Option[String] = d.o match {
    case PVLSourceNameOrigin(qualifiedName, _) => Some(qualifiedName)
    case o: JavaConstructorOrigin => Some(o.qualifiedName)
    case o: JavaMethodOrigin => Some(o.qualifiedName)
    case o: JavaInstanceFunctionOrigin => Some(o.qualifiedName)
    case o: JavaAnnotationMethodOrigin => Some(o.qualifiedName)
    case _ => None
  }

  override def dispatch(p: Program[Pre]): Program[Post] = {
    val (program, dropped) = filterAndAbstract[Pre](p, getFocused(p), getIgnored(p))

    def printKeeping[G](n: Node[G]): Unit = {
      val globalDecls = n.transSubnodes.collect({ case d: GlobalDeclaration[G] => d })
      val niceNamesKept = globalDecls.collect({ case ca: ContractApplicable[G] => getNiceName(ca) })
        .collect({ case Some(n) => n })
      val lessNiceNamesKept = globalDecls.collect({ case ca: ContractApplicable[G] => ca.o.preferredName })

      if (globalDecls.isEmpty) {
        // If there is nothing left, need to warn
        logger.warn("No declarations left after minimization!")
      } else if (niceNamesKept.nonEmpty) {
        // If there are some nice/clear names left, report those
        logger.info("Keeping:")
        niceNamesKept.foreach({ n => logger.info(s"- $n") })
        logger.info(s"and ${globalDecls.length - niceNamesKept.length} other global declarations")
      } else {
        // Otherwise, report less nice names
        logger.info("Keeping:")
        lessNiceNamesKept.foreach({ n => logger.info(s"- $n") })
      }
    }

    val droppedWithNiceName = dropped
      .collect({ case ca: ContractApplicable[Post] => getNiceName[Post](ca) })
      .collect({ case Some(n) => n })
    if (droppedWithNiceName.nonEmpty) {
      logger.info("Dropping:")
      droppedWithNiceName.foreach({ d => logger.info(s"- $d") })
      logger.info(s"and ${dropped.length - droppedWithNiceName.length} other declarations")
      printKeeping(program)
    } else {
      logger.info(s"Dropped ${dropped.length} declarations")
      printKeeping(program)
    }

    program
  }
}
