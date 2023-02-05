package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteProcedure, RewriteProgram}
import vct.col.ast.{ADTFunctionInvocation, AbstractMethod, AbstractPredicate, ApplicableContract, AxiomaticDataType, ContractApplicable, Declaration, Deref, Expr, FilterFocus, FilterIgnore, FilterIndicator, Fold, Function, FunctionInvocation, InstanceField, InstanceFunction, InstanceFunctionInvocation, InstanceMethod, InstancePredicate, InstancePredicateApply, InvokeMethod, InvokeProcedure, MethodInvocation, Predicate, PredicateApply, Procedure, ProcedureInvocation, Program, Statement, TAxiomatic, Type, Unfold, Unfolding}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.FilterDeclarations.{filterAndAbstract, ignoreToFocus}
import vct.col.util.AstBuildHelpers.{MethodBuildHelpers, PredicateBuildHelpers}

import scala.collection.mutable

// TODO (RR): Also make this work for the silver frontend. Then I can also do my original usecase, minimizing silver code, while also having an easily maintainable implementation for the java/pvl/c frontends

case object FilterDeclarations extends RewriterBuilder {
  override def key: String = "filterDeclarations"

  override def desc: String = "Filter focused declarations appropriately"

  // Returns true if focusing work needs to be done
  case class IgnoreToFocus[Pre <: Generation]() extends Rewriter[Pre]() {
    var containsFocus = false
    override def dispatch(p: Program[Pre]): Program[Post] = {
      val ignored = p.collect {
        case FilterIndicator(Ref(decl), FilterIgnore()) => decl
      }
      val focused = p.collect {
        case FilterIndicator(Ref(decl), FilterFocus()) => decl
      }

      // Focus overrides ignore, so only need to do work if there are only ignores
      if (focused.isEmpty && ignored.nonEmpty) {
        containsFocus = true

        val newFocused = p.collect {
          // Only select contractapplicables here, not sure how to handle non-contractapplicable decls
          case ca: ContractApplicable[Pre] if !ignored.contains(ca) => ca
        }.toSet

        p.rewrite(globalDeclarations.collect {
            p.declarations.foreach(dispatch(_))
            newFocused.foreach { ca: ContractApplicable[Pre] =>
              implicit val o = DiagnosticOrigin
              globalDeclarations.declare(FilterIndicator(anySucc[Declaration[Post]](ca.asInstanceOf[Declaration[Pre]]), FilterFocus()))
            }
          }._1)
      } else {
        containsFocus = focused.nonEmpty
        // Sneakily use the old program
        p.asInstanceOf[Program[Post]]
      }
    }
  }

  def ignoreToFocus[G <: Generation](p: Program[G]): (Program[Rewritten[G]], Boolean) = {
    val pass = IgnoreToFocus[G]()
    (pass.dispatch(p), pass.containsFocus)
  }
  case class AbstractMaker[Pre <: Generation](focusTargets: Seq[Declaration[Pre]]) extends Rewriter[Pre] {
    var program: Program[Pre] = null

    lazy val openedPredicates: Set[AbstractPredicate[Pre]] = program.collect {
      case Unfold(e) => getPredicate(e)
      case Fold(e) => getPredicate(e)
      case Unfolding(e, _) => getPredicate(e)
    }.toSet

    def getPredicate(e: Expr[Pre]): AbstractPredicate[Pre] = e match {
      case InstancePredicateApply(_, Ref(p), _, _) => p
      case PredicateApply(Ref(p), _, _) => p
    }

    override def dispatch(decl: Declaration[Pre]): Unit = {
      decl match {
        case m: AbstractMethod[Pre] if !focusTargets.contains(m) => allScopes.anySucceedOnly(m, m.rewrite(body = None))
        case p: AbstractPredicate[Pre] if !openedPredicates.contains(p) => allScopes.anySucceedOnly(p, p.rewrite(body = None))
        case d => super.dispatch(d)
      }
    }

    override def dispatch(program: Program[Pre]): Program[Post] = {
      this.program = program
      rewriteDefault(program)
    }
  }

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
          case pa: InstancePredicateApply[G] =>
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
          case ip: InvokeMethod[G] => collected.top.add(ip.ref.decl)
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
          // A decl using itself should not be counted as an actual use
          newCollected.remove(decl)
          // Same for an adt using its own functions
          decl match {
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

  case class RemoveUnused[Pre <: Generation](used: Seq[Declaration[Pre]]) extends Rewriter[Pre] with LazyLogging {
    var dropped: mutable.Set[Declaration[Pre]] = mutable.Set()

    def adtIsUsed(a: AxiomaticDataType[Pre]): Boolean =
      used.contains(a) || a.decls.exists(used.contains(_))

    override def dispatch(decl: Declaration[Pre]): Unit = {
      decl match {
        case _: Procedure[Pre] | _: Function[Pre] | _: Predicate[Pre] | _: InstancePredicate[Pre] | _: InstanceField[Pre] |
             _: InstanceMethod[Pre] | _: InstanceFunction[Pre]
          if !used.contains(decl) =>
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

case class FilterDeclarations[Pre <: Generation]() extends Rewriter[Pre]() with LazyLogging {
  override def dispatch(program: Program[Pre]): Program[Post] = {
    // Turn all ignore directives into inverted focus directives
    val (focusedProgram, containsFocus) = ignoreToFocus[Pre](program)

    if (!containsFocus) {
      return focusedProgram
    }

    val focused = focusedProgram.collect {
      case FilterIndicator(Ref(decl: ContractApplicable[Post]), FilterFocus()) => decl
    }

    val (filteredProgram, dropped) = filterAndAbstract[Post](focusedProgram, focused)

    if (dropped.nonEmpty) {
      logger.info(s"Dropped ${dropped.length} declarations")
    }

    filteredProgram.asInstanceOf[Program[Post]]
  }
}

