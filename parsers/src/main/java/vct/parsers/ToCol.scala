package vct.parsers

import hre.ast.FileOrigin
import hre.lang.HREExitException
import org.antlr.v4.runtime.ParserRuleContext
import vct.col.ast.{ApplicableContract, Expr, Origin, Star, Type, Variable}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Contract
import vct.col.ast.util.{ASTFactory, ContractBuilder}

import scala.collection.mutable
import scala.reflect.ClassTag

abstract class ToCol(val originProvider: OriginProvider) {
  class ContractCollector() {
    val modifies: mutable.ArrayBuffer[(ParserRuleContext, String)] = mutable.ArrayBuffer()
    val accessible: mutable.ArrayBuffer[(ParserRuleContext, String)] = mutable.ArrayBuffer()
    val signals: mutable.ArrayBuffer[(ParserRuleContext, (Variable, Expr))] = mutable.ArrayBuffer()

    val requires: mutable.ArrayBuffer[(ParserRuleContext, Expr)] = mutable.ArrayBuffer()
    val ensures: mutable.ArrayBuffer[(ParserRuleContext, Expr)] = mutable.ArrayBuffer()
    val context_everywhere: mutable.ArrayBuffer[(ParserRuleContext, Expr)] = mutable.ArrayBuffer()
    val kernel_invariant: mutable.ArrayBuffer[(ParserRuleContext, Expr)] = mutable.ArrayBuffer()

    val given: mutable.ArrayBuffer[(ParserRuleContext, Variable)] = mutable.ArrayBuffer()
    val yields: mutable.ArrayBuffer[(ParserRuleContext, Variable)] = mutable.ArrayBuffer()

    val loop_invariant: mutable.ArrayBuffer[(ParserRuleContext, Expr)] = mutable.ArrayBuffer()

    def consume[T](buffer: mutable.ArrayBuffer[(ParserRuleContext, T)]): Seq[T] = {
      val result = buffer.map(_._2)
      buffer.clear()
      result
    }

    def consumeApplicableContract()(implicit o: Origin): ApplicableContract = {
      ApplicableContract(Star.fold(consume(requires)), Star.fold(consume(ensures)),
                         Star.fold(consume(context_everywhere)),
                         consume(signals), consume(given), consume(yields))
    }

    def nodes: Seq[ParserRuleContext] = Seq(
      modifies, accessible, signals,
      requires, ensures, context_everywhere, kernel_invariant,
      given, yields, loop_invariant,
    ).flatMap(_.map(_._1))
  }

  class ModifierCollector() {
    val pure: mutable.ArrayBuffer[ParserRuleContext] = mutable.ArrayBuffer()
    val inline: mutable.ArrayBuffer[ParserRuleContext] = mutable.ArrayBuffer()
    val threadLocal: mutable.ArrayBuffer[ParserRuleContext] = mutable.ArrayBuffer()
    val static: mutable.ArrayBuffer[ParserRuleContext] = mutable.ArrayBuffer()

    def consume(buffer: mutable.ArrayBuffer[ParserRuleContext]): Boolean = {
      val result = buffer.nonEmpty
      buffer.clear()
      result
    }

    def nodes: Seq[ParserRuleContext] = Seq(pure, inline, threadLocal, static).flatten
  }

  def convertList[Input, Append <: Input, Singleton <: Input, Element]
                 (extractSingle: Singleton => Option[Element], extractAppend: Append => Option[(Input, Element)])
                 (input: Input)
                 (implicit singleTag: ClassTag[Singleton], appendTag: ClassTag[Append]): Seq[Element] =
    input match {
      case single: Singleton => Seq(extractSingle(single).get)
      case append: Append => extractAppend(append).get match {
        case (input, element) => convertList(extractSingle, extractAppend)(input) :+ element
      }
    }

  def getOrFail[B](node: ParserRuleContext, thing: Either[String, B]): B = thing match {
    case Left(err) => fail(node, err)
    case Right(good) => good
  }

  def getOrFail[B](node: ParserRuleContext, thing: Option[B], message: String): B = thing match {
    case None => fail(node, message)
    case Some(b) => b
  }

  def failIfDefined[T <: ParserRuleContext](node: Option[T], format: String): Unit = node match {
    case Some(node) => fail(node, format)
    case None => // do nothing
  }

  def fail(tree: ParserRuleContext, message: String): Nothing = {
    throw ParseError(originProvider(tree), message)
  }

  /**
   * Print notice and exit, because a rule is unimplemented in the conversion to COL. Named after the scala
   * "unimplemented" method, [[???]]
   */
  def ??(tree: ParserRuleContext): Nothing = {
    fail(tree, f"This construct (${tree.getClass.getSimpleName}) is syntactically valid, but not supported by VerCors.")
  }
}
