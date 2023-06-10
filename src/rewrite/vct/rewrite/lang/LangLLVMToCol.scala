package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.{BlameCollector, LLVMOrigin, Origin, InvocationFailure}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.origin.RedirectOrigin.StringReadable
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.ctx.RefLlvmFunctionDefinition
import vct.col.util.SuccessionMap

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val functionMap: SuccessionMap[LlvmFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()


  def rewriteLocal(local: LlvmLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Local(rw.succ(local.ref.get.decl))
  }

  def rewriteFunctionDef(func: LlvmFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o
    val procedure = rw.labelDecls.scope {
      rw.globalDeclarations.declare(
        new Procedure[Post](
          returnType = rw.dispatch(func.returnType),
          args = rw.variables.collect {
            func.args.foreach(rw.dispatch)
          }._1,
          outArgs = Nil,
          typeArgs = Nil,
          body = Some(rw.dispatch(func.functionBody)),
          contract = rw.dispatch(func.contract.data.get),
          pure = func.pure
        )(func.blame)
      )
    }
    functionMap.update(func, procedure)
  }

  def rewriteAmbiguousFunctionInvocation(inv: LlvmAmbiguousFunctionInvocation[Pre]): ProcedureInvocation[Post] = {
    implicit val o: Origin = inv.o
    new ProcedureInvocation[Post](
      ref = new LazyRef[Post, Procedure[Post]](functionMap(inv.ref.get.decl)),
      args = inv.args.map(rw.dispatch),
      givenMap = inv.givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
      yields = inv.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
      outArgs = Seq.empty,
      typeArgs = Seq.empty
    )(inv.blame)
  }

  def rewriteFunctionInvocation(inv: LlvmFunctionInvocation[Pre]): ProcedureInvocation[Post] = {
    implicit val o: Origin = inv.o
    new ProcedureInvocation[Post](
      ref = new LazyRef[Post, Procedure[Post]](functionMap(inv.ref.decl)),
      args = inv.args.map(rw.dispatch),
      givenMap = inv.givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
      yields = inv.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
      outArgs = Seq.empty,
      typeArgs = Seq.empty
    )(inv.blame)
  }

  def result(ref: RefLlvmFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](functionMap.ref(ref.decl))
}