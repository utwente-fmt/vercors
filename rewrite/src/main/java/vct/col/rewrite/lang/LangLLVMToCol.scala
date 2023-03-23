package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewritten}

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw


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


    rw.globalDeclarations.declare(
      new Procedure[Post](
        returnType = rw.dispatch(func.returnType),
        args = rw.variables.collect {
          func.args.foreach(rw.dispatch)
        }._1,
        outArgs = Nil,
        typeArgs = Nil,
        body = Some(rw.dispatch(func.body)),
        contract = stubContract
      )(func.blame)(func.o)
    )
  }

}
