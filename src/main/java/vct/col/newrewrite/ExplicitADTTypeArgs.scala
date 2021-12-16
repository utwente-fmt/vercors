package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast._
import vct.col.newrewrite.ExplicitADTTypeArgs.UnspecifiedADTTypeArgs
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationResult.UserError

import scala.collection.mutable

case object ExplicitADTTypeArgs extends RewriterBuilder {
  case class UnspecifiedADTTypeArgs(inv: ADTFunctionInvocation[_]) extends UserError {
    override def code: String = "typeArgs"
    override def text: String =
      inv.o.messageInContext("Missing type parameters for ADT function invocation.")
  }
}

case class ExplicitADTTypeArgs[Pre <: Generation]() extends Rewriter[Pre] {
  val owner: mutable.Map[ADTFunction[Pre], AxiomaticDataType[Pre]] = mutable.Map()
  val currentAdt: ScopedStack[AxiomaticDataType[Pre]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    program.declarations.foreach {
      case adt: AxiomaticDataType[Pre] => adt.decls.foreach {
        case func: ADTFunction[Pre] => owner(func) = adt
        case _ =>
      }
      case _ =>
    }
    program.rewrite()
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case adt: AxiomaticDataType[Pre] =>
      currentAdt.having(adt) {
        rewriteDefault(adt)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case specified@ADTFunctionInvocation(Some(_), _, _) => rewriteDefault(specified)
    case unspecified@ADTFunctionInvocation(None, Ref(func), args) =>
      if (currentAdt.nonEmpty && owner(func) == currentAdt.top) {
        ADTFunctionInvocation[Post](
          typeArgs = Some((succ(owner(func)), owner(func).typeArgs.map(v => TVar(succ[Variable[Post]](v))))),
          ref = succ(func),
          args = args.map(dispatch),
        )(e.o)
      } else {
        owner(func).typeArgs match {
          case Nil => ADTFunctionInvocation[Post](Some((succ(owner(func)), Nil)), succ(func), args.map(dispatch))(e.o)
          case _ => throw UnspecifiedADTTypeArgs(unspecified)
        }
      }
    case other => rewriteDefault(other)
  }
}
