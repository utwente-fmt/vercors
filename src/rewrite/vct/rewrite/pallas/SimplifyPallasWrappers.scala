package vct.rewrite.pallas

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{CurrentRewriteProgramContext, SuccessionMap}
import vct.result.VerificationError
import vct.rewrite.EncodeByValueClassUsage.UnsupportedStructPerm

import scala.collection.mutable

case object SimplifyPallasWrappers extends RewriterBuilder {
  override def key: String = "simplifyPallasWrappers"

  override def desc: String =
    "Simplify wrapper functions of Pallas specifications in preparation if the statement-to-expression conversion."
}

/** Prepares the wrapper functions of Pallas-Specifications for the
  * statement-to-expression transformation. performs the following
  * simplifications:
  *   - Pointer-typed locals that are only ever accessed through a deref are
  *     lowered to non-pointer variables
  *   - Wrapper-functions and calls to wrapper functions often contain the
  *     following patterns ´*&arg_XX´ which is simplified to ´arg_XX´.
  */

case class SimplifyPallasWrappers[Pre <: Generation]() extends Rewriter[Pre] {
  import SimplifyPallasWrappers._

  private val ptrLocalsToLower: ScopedStack[Set[Variable[Pre]]] = ScopedStack()

  // Keeps track if rewrite is currently in a Pallas wrapper-function:
  var inPallasSpec: ScopedStack[Boolean] = ScopedStack()

  var inWrapperCall: ScopedStack[Boolean] = ScopedStack()

  private def inSpec(): Boolean = { inPallasSpec.nonEmpty && inPallasSpec.top }

  private def inWCall(): Boolean = {
    inWrapperCall.nonEmpty && inWrapperCall.top
  }

  private def shouldLower(v: Variable[Pre]): Boolean = {
    ptrLocalsToLower.nonEmpty && ptrLocalsToLower.top.contains(v)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case p: Procedure[Pre] =>
        inPallasSpec.having(p.pallasWrapper) { super.dispatch(decl) }
      case p: LLVMPredicateDefinition[Pre] =>
        inPallasSpec.having(true) { super.dispatch(decl) }
      case v: Variable[Pre] if inSpec() && shouldLower(v) =>
        // Remove pointer from type of lowered local
        val innerT = v.t match { case TPointer(inner, _) => inner }
        variables.succeed(v, new Variable[Post](dispatch(innerT)))
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = node.o

    if (!inSpec()) { return super.dispatch(node) }

    node match {
      case s @ Scope(vars, impl) =>
        // Check if locals of the scope can be lowered
        val lowerable = getLowerableLocals(s)
        ptrLocalsToLower.having(
          lowerable.union(ptrLocalsToLower.topOption.getOrElse(Set.empty))
        ) { s.rewriteDefault() }
      case _ => node.rewriteDefault()
    }
  }

  private def getLowerableLocals(scope: Scope[Pre]): Set[Variable[Pre]] = {
    // Check if local variables of the scope can be lowered
    scope.locals.toSet.filter { v =>
      varOnlyUsedWithDeref(scope, v) && getVarAssignments(scope, v).size == 1
      // TODO: Also check if the assignment occurs before the read?
    }
  }

  /** Check if variable ´v´ is only used within a PointerDeref.
    */
  private def varOnlyUsedWithDeref(
      s: Statement[Pre],
      v: Variable[Pre],
  ): Boolean = {
    val usedDirectly = s.collectShallow {
      case DerefPointer(Local(Ref(v2))) if v == v2 => true
      case Local(Ref(v2)) if v == v2 => false
      case inv: InvokingNode[Pre]
          if inv.givenMap.exists(_._1.decl == v) ||
            inv.yields.exists(_._2.decl == v) =>
        false
    }.contains(false)
    !usedDirectly
  }

  private def getVarAssignments(
      s: Statement[Pre],
      v: Variable[Pre],
  ): Seq[Node[Pre]] = {
    val ass = s.collect {
      case a: AssignStmt[Pre] if a.target.exists {
            case Local(Ref(v2)) if v2 == v => true
          } =>
        a
      case inv: InvokingNode[Pre] if inv.yields.exists(_._2.decl == v) => inv
    }
    ass
  }

  override def dispatch(node: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = node.o

    if (inSpec()) {
      // In Specification
      node match {
        // Remove the deref around lowered locals.
        case DerefPointer(l @ Local(Ref(v))) if shouldLower(v) =>
          l.rewriteDefault()
        case DerefPointer(AddrOf(l: Local[Pre])) => l.rewriteDefault()
        case _ => node.rewriteDefault()
      }
    } else if (inWCall()) {
      // In call to wrapper function
      node match {
        case DerefPointer(AddrOf(l: Local[Pre])) => l.rewriteDefault()
        case _ => node.rewriteDefault()
      }
    } else {
      // Somewhere else
      node match {
        case inv: ProcedureInvocation[Pre] =>
          inWrapperCall.having(inv.ref.decl.pallasWrapper) {
            inv.rewriteDefault()
          }
        case _ => super.dispatch(node)
      }
    }
  }

}
