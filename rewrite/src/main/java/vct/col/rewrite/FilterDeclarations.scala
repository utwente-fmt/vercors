package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteProcedure, RewriteProgram}
import vct.col.ast.{ADTFunctionInvocation, AbstractMethod, AxiomaticDataType, ContractApplicable, Declaration, Deref, Expr, FilterIndicator, Function, FunctionInvocation, InstanceField, InstanceFunction, InstanceFunctionInvocation, InstanceMethod, InvokeProcedure, MethodInvocation, Predicate, PredicateApply, Procedure, ProcedureInvocation, Program, Statement, TAxiomatic, Type}
import vct.col.ref.Ref
import vct.col.rewrite.FilterDeclarations.filterAndAbstract
import vct.col.util.AstBuildHelpers.MethodBuildHelpers

import scala.collection.mutable

// TODO (RR): Don't think I abstract non-opened predicates here
// TODO (RR): What about unfolding a predicate? Should we abstract it if it is not unfolded?
// TODO (RR): Also make this work for the silver frontend. Then I can also do my original usecase, minimizing silver code, while also having an easily maintainable implementation for the java/pvl/c frontends

case object FilterDeclarations extends RewriterBuilder {
  override def key: String = "filterDeclarations"

  override def desc: String = "Filter focused declarations appropriately"

  // Get all predicate usages, method usages, field usages, adt usages, adt function usages
  def getUsedDecls[G <: Generation](p: Program[G]): Seq[Declaration[G]] = {
    val collected = ScopedStack[mutable.Set[Declaration[G]]]()

    case class Collector() extends Rewriter[G] {
      override def dispatch(expr: Expr[G]): Expr[Rewritten[G]] = {
        expr match {
          case pi: ProcedureInvocation[G] =>
            collected.top.add(pi.ref.decl)
          case fi: FunctionInvocation[G] =>
            collected.top.add(fi.ref.decl)
          case ipi: MethodInvocation[G] =>
            collected.top.add(ipi.ref.decl)
          case ifi: InstanceFunctionInvocation[G] =>
            collected.top.add(ifi.ref.decl)
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
        case _: Procedure[G] | _: Predicate[G] | _: Function[G] | _: AxiomaticDataType[G] | _: InstanceMethod[G] | _: InstanceFunction[G] =>
          val newCollected = mutable.Set[Declaration[G]]()
          collected.having(newCollected) {
            super.dispatch(decl)
          }
          // A function using itself should not be counted as an actual use
          newCollected.remove(decl)
          // Same for an adt using its own functions
          decl match {
            // TODO: Is this going okay here? I did not really think it through when refactoring...
            case a: AxiomaticDataType[G] => a.decls.foreach(f => newCollected.remove(f))
            case _ =>
          }
          collected.top.addAll(newCollected)
        case d =>
          super.dispatch(d)
      }
    }

    val res = mutable.Set[Declaration[G]]()
    collected.having(res) {
      Collector().dispatch(p)
    }
    res.toSeq
  }

  def filterAndAbstract[G <: Generation](p: Program[G], inputFocused: Seq[ContractApplicable[G]]): (Program[Rewritten[G]], Seq[Declaration[_]]) = {
    val am = AbstractMaker(inputFocused)
    var program = am.dispatch(p)
    var focused = inputFocused.map(am.anySucc[Declaration[Rewritten[G]]](_).decl)
    var dropped: Seq[Declaration[_]] = Nil
    var totalDropped: Seq[Declaration[_]] = Nil

    do {
      val ru = RemoveUnused[Rewritten[G]](getUsedDecls(program) ++ focused)
      val reducedProgram = ru.dispatch(program)
      program = reducedProgram.asInstanceOf[Program[Rewritten[G]]]
      focused = focused.map(ru.anySucc[Declaration[Rewritten[Rewritten[G]]]](_).decl).asInstanceOf[Seq[Declaration[Rewritten[G]]]]
      dropped = ru.dropped.toSeq
      totalDropped ++= dropped
    } while (dropped.nonEmpty)

    (program, totalDropped)
  }
}

case class AbstractMaker[Pre <: Generation](focusTargets: Seq[Declaration[Pre]]) extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case m: AbstractMethod[Pre] if !focusTargets.contains(m) => allScopes.anySucceedOnly(m, m.rewrite(body = None))
      case d => super.dispatch(d)
    }
  }
}

case class RemoveUnused[Pre <: Generation](used: Seq[Declaration[Pre]]) extends Rewriter[Pre] with LazyLogging {
  var dropped: mutable.Set[Declaration[Pre]] = mutable.Set()

  def adtIsUsed(a: AxiomaticDataType[Pre]): Boolean =
    used.contains(a) || a.decls.exists(used.contains(_))

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case _: Procedure[Pre] | _: Function[Pre] | _: Predicate[Pre] | _: InstanceField[Pre] if !used.contains(decl) =>
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

case class FilterDeclarations[Pre <: Generation]() extends Rewriter[Pre]() with LazyLogging {
  override def dispatch(p: Program[Pre]): Program[Post] = {
    val focused = p.collect {
      case FilterIndicator(Ref(decl: ContractApplicable[Pre]), true, false) => decl
    }.toIndexedSeq

    if (focused.isEmpty) {
      return p.rewrite()
    }

    val (program, dropped) = filterAndAbstract[Pre](p, focused)

    if (dropped.nonEmpty) {
      logger.info(s"Dropped ${dropped.length} declarations")
    }

    program
  }
}

