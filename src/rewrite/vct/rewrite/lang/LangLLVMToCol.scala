package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.{InvocationFailure, Origin}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val functionMap: SuccessionMap[LlvmFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()


  def rewriteFunctionDef(func: LlvmFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.contract.o
    // TODO replace stub contract
    val stubContract = new ApplicableContract[Post](
      requires = new UnitAccountedPredicate[Post](BooleanValue(value = true)),
      ensures = new UnitAccountedPredicate[Post](BooleanValue(value = true)),
      contextEverywhere = BooleanValue(value = true),
      signals = Seq.empty,
      givenArgs = Seq.empty,
      yieldsArgs = Seq.empty,
      decreases = None)(func.contract.blame)(func.contract.o)

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
          contract = stubContract
        )(func.blame)(func.o)
      )
    }
    functionMap.update(func, procedure)
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
}