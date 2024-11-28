package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  ADTFunctionInvocation,
  AbstractMethod,
  AbstractPredicate,
  ApplicableContract,
  AxiomaticDataType,
  ConstructorInvocation,
  ContractApplicable,
  Declaration,
  Deref,
  Exclude,
  Expr,
  FilterApplicable,
  FilterMode,
  Fold,
  FoldTarget,
  Function,
  FunctionInvocation,
  Include,
  InlineableApplicable,
  InstanceField,
  InstanceFunction,
  InstanceFunctionInvocation,
  InstanceMethod,
  InstancePredicate,
  InstancePredicateApply,
  InvokeConstructor,
  InvokeMethod,
  InvokeProcedure,
  MethodInvocation,
  NeutralFilterMode,
  Predicate,
  PredicateApply,
  Procedure,
  ProcedureInvocation,
  Program,
  Statement,
  TAxiomatic,
  Type,
  Unfold,
  Unfolding,
}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.{MethodBuildHelpers, PredicateBuildHelpers}
import vct.rewrite.FilterDeclarations.{filterAndAbstract, toIncludeOnly}

import scala.collection.mutable

/* TODO: Here ADTs are handled specially, such that they are removed if they are unused.
    This is not ideal, as ADT axioms might still influence program verification.
    In addition, I don't think removing an ADT can enable removing of other callables (method, predicate, etc.)
    So optimizing unused ADTs away should be a separate pass (or triggered by a --minimize flag or smth)

   TODO: Add a pass that optimizes unused ADTs away
 */
case object FilterDeclarations extends RewriterBuilder {
  override def key: String = "filterDeclarations"

  override def desc: String =
    "Filter declarations based on inclusion/exclusion attributes"

  case class InvertFilterModes[Pre <: Generation]() extends Rewriter[Pre]() {
    override def dispatch(filterMode: FilterMode[Pre]): FilterMode[Post] =
      filterMode match {
        case Include() => Exclude()(filterMode.o)
        case Exclude() => Include()(filterMode.o)
        case _ => filterMode.rewriteDefault()
      }
  }

  case class DropExcludes[Pre <: Generation]() extends Rewriter[Pre]() {
    override def dispatch(filterMode: FilterMode[Pre]): FilterMode[Post] =
      filterMode match {
        case Exclude() => NeutralFilterMode()
        case _ => filterMode.rewriteDefault()
      }
  }

  case class ToIncludeOnly[Pre <: Generation]() extends Rewriter[Pre]() {
    // Returns true if there are Include nodes in the program, and hence filtering needs to be done
    def rewrite(program: Program[Pre]): (Program[Post], Boolean) = {
      val excluded = program.collect {
        case app: FilterApplicable[Pre] if app.filterMode == Some(Exclude()) =>
          app
      }
      val included = program.collect {
        case app: FilterApplicable[Pre] if app.filterMode == Some(Include()) =>
          app
      }

      (included, excluded) match {
        // No includes, excludes, program is in include-only form. Nothing to transform
        case (Seq(), Seq()) => (program.asInstanceOf[Program[Post]], false)
        // Only includes: program is already in include-only form. Nothing to transform
        case (includes, Seq()) => (program.asInstanceOf[Program[Post]], true)
        // Only ignores: invert filtermodes to "include", to only have includes of all non-annotated declarations
        case (Seq(), ignored) => (InvertFilterModes().dispatch(program), true)
        // Both includes/excludes: just drop the excludes, as the includes will trigger deletion of everything else
        // In theory the excludes could be left in. However, the invariant that after this pass there are only includes
        // and neutrals is nice.
        case (_, _) => (DropExcludes().dispatch(program), true)
      }
    }
  }

  def toIncludeOnly[G <: Generation](
      p: Program[G]
  ): (Program[Rewritten[G]], Boolean) = ToIncludeOnly[G]().rewrite(p)

  case class AbstractMaker[Pre <: Generation](
      includes: Set[FilterApplicable[Pre]]
  ) extends Rewriter[Pre] {
    var program: Program[Pre] = null

    lazy val openedPredicates: Set[AbstractPredicate[Pre]] =
      program.collect {
        case Unfold(e) => e.apply.ref.decl
        case Fold(e) => e.apply.ref.decl
        case Unfolding(e, _) => e.apply.ref.decl
      }.toSet

    override def dispatch(decl: Declaration[Pre]): Unit = {
      decl match {
        case m: AbstractMethod[Pre] if !includes.contains(m) =>
          allScopes.anySucceedOnly(m, m.rewrite(body = None))
        case p: AbstractPredicate[Pre] if !openedPredicates.contains(p) =>
          allScopes.anySucceedOnly(p, p.rewrite(body = None))
        case d => super.dispatch(d)
      }
    }

    override def dispatch(program: Program[Pre]): Program[Post] = {
      this.program = program
      program.rewriteDefault()
    }
  }

  // Get all predicate usages, method usages, field usages, adt usages, adt function usages
  // Messy because usages internal to the type (e.g. using an adt function in an axiom) should be ignored
  def getUsedFilterApplicables[G <: Generation](
      p: Program[G]
  ): Set[FilterApplicable[G]] = {
    val collected = ScopedStack[mutable.Set[Declaration[G]]]()

    case class Collector() extends Rewriter[G] {
      def decls: mutable.Set[Declaration[G]] = collected.top

      override def dispatch(expr: Expr[G]): Expr[Rewritten[G]] = {
        expr match {
          // This whole match statement could be factored out into a common Uses[Declaration] trait.
          // Or maybe reuse the Invocation/InvokingNode hierarchy?
          // Then this whole getUsedDecls method could be generic. But I dislike increasing trait pressure in Node.scala
          case pi: ProcedureInvocation[G] => decls.add(pi.ref.decl)
          case fi: FunctionInvocation[G] => decls.add(fi.ref.decl)
          case ipi: MethodInvocation[G] => decls.add(ipi.ref.decl)
          case ifi: InstanceFunctionInvocation[G] => decls.add(ifi.ref.decl)
          case pa: PredicateApply[G] => decls.add(pa.ref.decl)
          case pa: InstancePredicateApply[G] => decls.add(pa.ref.decl)
          case afi: ADTFunctionInvocation[G] => decls.add(afi.ref.decl)
          case inv: ConstructorInvocation[G] => decls.add(inv.ref.decl)
          case Deref(_, r) => decls.add(r.decl)
          case _ =>
        }
        super.dispatch(expr)
      }

      // Just in case this pass is moved, let's match on the low-level statements as well
      override def dispatch(s: Statement[G]): Statement[Rewritten[G]] = {
        s match {
          case ip: InvokeProcedure[G] => decls.add(ip.ref.decl)
          case ip: InvokeMethod[G] => decls.add(ip.ref.decl)
          case inv: InvokeConstructor[G] => decls.add(inv.ref.decl)
          case _ =>
        }
        super.dispatch(s)
      }

      override def dispatch(declaration: Declaration[G]): Unit =
        declaration match {
          case app: FilterApplicable[G] =>
            val newDecls = mutable.Set[Declaration[G]]()
            collected.having(newDecls) { super.dispatch(declaration) }
            // A decl using itself should not be counted as an actual use
            newDecls.remove(declaration)
            decls.addAll(newDecls)
          case d => super.dispatch(d)
        }
    }

    val decls = mutable.Set[Declaration[G]]()
    collected.having(decls) { Collector().dispatch(p) }
    decls.toSet
  }

  case class RemoveUnused[Pre <: Generation](used: Set[FilterApplicable[Pre]])
      extends Rewriter[Pre] with LazyLogging {
    var dropped: mutable.Set[Declaration[Pre]] = mutable.Set()
    lazy val usedSet = used.toSet

    override def dispatch(decl: Declaration[Pre]): Unit = {
      decl match {
        case app: FilterApplicable[Pre] if !usedSet.contains(app) =>
          app.drop()
          dropped.add(app)
        case _ => decl.rewriteDefault()
      }
    }
  }

  def filterAndAbstract[G <: Generation](
      program: Program[G],
      included: Set[FilterApplicable[G]],
  ): Program[_] = {
    val am = AbstractMaker(included)
    val abstractProgram = am.dispatch(program)
    // Maintain set of applicables to retain through the transformation
    val includedAbstract = included
      .map(am.anySucc[FilterApplicable[Rewritten[G]]](_).decl)

    val removeUnused = RemoveUnused[Rewritten[G]](
      getUsedFilterApplicables(abstractProgram).union(includedAbstract)
    )
    val reducedProgram = removeUnused.dispatch(abstractProgram)

    if (removeUnused.dropped.nonEmpty) {
      filterAndAbstract(reducedProgram, includedAbstract)
    } else { reducedProgram }
  }
}

case class FilterDeclarations[Pre <: Generation]()
    extends Rewriter[Pre]() with LazyLogging {
  override def dispatch(program: Program[Pre]): Program[Post] = {
    // Remove exclude directions by making everything else include
    val (includeOnlyProgram, containsInclude) = toIncludeOnly[Pre](program)

    // If there are no includes, stop early
    if (!containsInclude) { return includeOnlyProgram }

    val includes = includeOnlyProgram.collect {
      case app: FilterApplicable[Post] if app.filter == Include[Post]() => app
    }

    val filteredProgram = filterAndAbstract(includeOnlyProgram, includes)

    filteredProgram.asInstanceOf[Program[Post]]
  }
}
