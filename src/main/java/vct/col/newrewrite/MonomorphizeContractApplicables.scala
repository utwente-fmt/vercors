package vct.col.newrewrite

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.newrewrite.MonomorphizeContractApplicables.VerificationForGeneric
import vct.col.newrewrite.util.Substitute
import vct.col.origin.Origin
import vct.col.ref.{DirectRef, LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.ContractApplicableBuildHelpers
import vct.col.util.SuccessionMap

import scala.collection.mutable
import scala.reflect.ClassTag

case object MonomorphizeContractApplicables extends RewriterBuilder {
  case class VerificationForGeneric(applicable: ContractApplicable[_]) extends Origin {
    override def preferredName: String = "verify_" + applicable.o.preferredName
    override def context: String = applicable.o.context
  }
}

case class MonomorphizeContractApplicables[Pre <: Generation]() extends Rewriter[Pre] {
  val currentSubstitutions: ScopedStack[Map[Variable[Pre], Type[Post]]] = ScopedStack()

  val monomorphized: mutable.Map[Seq[Type[Post]], ContractApplicable[Post]] = mutable.Map()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: ContractApplicable[Pre] if app.typeArgs.nonEmpty =>
      implicit val o: Origin = decl.o
      app.typeArgs.foreach(_.drop())
      val typeValues = app.typeArgs.map(v => dispatch(v.t.asInstanceOf[TType[Pre]].t))
      currentSubstitutions.having(app.typeArgs.zip(typeValues).toMap) {
        app.rewrite(typeArgs = Nil).succeedDefault(app)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case inv: Invocation[Pre] if inv.ref.decl.typeArgs.nonEmpty =>
      val typeValues = inv.typeArgs.map(dispatch)

      val app = monomorphized.getOrElseUpdate(typeValues, currentSubstitutions.having(inv.ref.decl.typeArgs.zip(typeValues).toMap) {
        freshSuccessionScope {
          val app1 = inv.ref.decl.rewrite(typeArgs = Nil)
          app1.declareDefault(this)
          app1
        }
      })

      inv match {
        case inv: ProcedureInvocation[Pre] => inv.rewrite(ref = new DirectRef(app), typeArgs = Nil)
        case inv: MethodInvocation[Pre] => inv.rewrite(ref = new DirectRef(app), typeArgs = Nil)
        case inv: FunctionInvocation[Pre] => inv.rewrite(ref = new DirectRef(app), typeArgs = Nil)
        case inv: InstanceFunctionInvocation[Pre] => inv.rewrite(ref = new DirectRef(app), typeArgs = Nil)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Rewritten[Pre]] = t match {
    case TVar(Ref(v)) =>
      currentSubstitutions.topOption.getOrElse(Map.empty[Variable[Pre], Type[Post]]).getOrElse(v, TVar(succ(v)))
    case other => rewriteDefault(other)
  }
}
