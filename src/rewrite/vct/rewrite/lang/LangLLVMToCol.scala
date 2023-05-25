package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import vct.col.ast._
import vct.col.origin.{BlameCollector, LLVMOrigin, Origin}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.origin.RedirectOrigin.StringReadable
import vct.parsers.ColLLVMParser
import vct.parsers.transform.{ConstantBlameProvider, ReadableOriginProvider}

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw


  def rewriteFunctionDef(func: LlvmFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.contract.o
    rw.labelDecls.scope {
      rw.globalDeclarations.declare(
        new Procedure[Post](
          returnType = rw.dispatch(func.returnType),
          args = rw.variables.collect {
            func.args.foreach(rw.dispatch)
          }._1,
          outArgs = Nil,
          typeArgs = Nil,
          body = Some(rw.dispatch(func.functionBody)),
          contract = rewriteLLVMFunctionContract(func.contract)
        )(func.blame)(func.o)
      )
    }
  }

  def rewriteLLVMFunctionContract(llvmContract: LlvmFunctionContract[Pre]): ApplicableContract[Post] = {
    val originProvider = ReadableOriginProvider(llvmContract.o match {
      case o: LLVMOrigin => StringReadable(llvmContract.value, o.fileName)
      case _ => StringReadable(llvmContract.value)
    })
    val charStream = CharStreams.fromString(llvmContract.value)
    val contract = ColLLVMParser(originProvider, ConstantBlameProvider(BlameCollector()))
      .parseFunctionContract[Pre](charStream, llvmContract.references)._1
    rw.dispatch(contract)
  }
}