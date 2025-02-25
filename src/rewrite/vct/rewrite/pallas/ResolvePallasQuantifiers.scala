package vct.rewrite.pallas

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError
import vct.rewrite.pallas.ResolvePallasQuantifiers.{
  InvalidBoundVariableUsage,
  MissingBoundVariable,
}

object ResolvePallasQuantifiers extends RewriterBuilder {
  override def key: String = "resolvePallasQuantifiers"
  override def desc: String = "Encodes pallas-quantifiers into col-quantifiers."

  private final case class InvalidBoundVariableUsage(bv: LLVMBoundVar[_])
      extends UserError {
    override def code: String = "llvmInvalidBoundVarUsage"

    override def text: String =
      bv.o.messageInContext(f"Unsupported use of a bound variable: $bv")
  }

  private final case class MissingBoundVariable(q: LLVMQuantifier[_])
      extends UserError {
    override def code: String = "llvmMissingBoundVariable"

    override def text: String =
      q.o.messageInContext(
        f"The quantifier does not introduce any new bound variables: $q"
      )
  }
}

case class ResolvePallasQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {

  // Tracks the bound-variables that are available in the current quantifier.
  private val boundVars: ScopedStack[Map[(String, Type[Pre]), Variable[Post]]] =
    ScopedStack()

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {
    expr match {
      case q @ LLVMForall(bindingExpr, bodyExpr) =>
        implicit val o: Origin = expr.o
        val newBVMap = gatherBoundVars(q)
        val newBindings = newBVMap.values.toSeq
        boundVars.having(newBVMap) {
          Forall[Post](
            newBindings,
            Seq.empty,
            Implies(dispatch(bindingExpr), dispatch(bodyExpr)),
          )
        }
      case q @ LLVMSepForall(bindingExpr, bodyExpr) =>
        implicit val o: Origin = expr.o
        val newBVMap = gatherBoundVars(q)
        val newBindings = newBVMap.values.toSeq
        boundVars.having(newBVMap) {
          Starall[Post](
            newBindings,
            Seq.empty,
            Implies(dispatch(bindingExpr), dispatch(bodyExpr)),
          )(q.blame)
        }
      case q @ LLVMExists(bindingExpr, bodyExpr) =>
        implicit val o: Origin = expr.o
        val newBVMap = gatherBoundVars(q)
        val newBindings = newBVMap.values.toSeq
        boundVars.having(newBVMap) {
          Exists[Post](
            newBindings,
            Seq.empty,
            Implies(dispatch(bindingExpr), dispatch(bodyExpr)),
          )
        }
      case bv @ LLVMBoundVar(id, varType) =>
        implicit val o: Origin = bv.o
        val bvMap = boundVars.topOption.getOrElse(Map.empty)
        val bindingVar = bvMap
          .getOrElse((id, varType), throw InvalidBoundVariableUsage(bv))
        Local(bindingVar.ref)
      case _ => expr.rewriteDefault()
    }
  }

  /** Gathers the bound variables that are available in a quantifier with the
    * given binding expression.
    */
  private def gatherBoundVars(
      q: LLVMQuantifier[Pre]
  ): Map[(String, Type[Pre]), Variable[Post]] = {
    val oldBVMap = boundVars.topOption.getOrElse(Map.empty)
    // Gather all new bound variables from the binding expression
    val bVars = q.bindingExpr.collect { case LLVMBoundVar(id, vType) =>
      (id, vType)
    }.filterNot(t => oldBVMap.contains(t))

    // Declare variables for new BVs & update stack
    var newBVMap = oldBVMap
    variables.scope {
      localHeapVariables.scope {
        bVars.foreach { case (id, vType) =>
          val v = new Variable[Post](dispatch(vType))(q.o.where(name = id))
          newBVMap = newBVMap.updated((id, vType), v)
        }
      }
    }

    if (newBVMap.isEmpty) { throw MissingBoundVariable(q) }
    newBVMap
  }

}
