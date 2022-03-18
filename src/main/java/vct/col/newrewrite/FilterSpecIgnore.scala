package vct.col.newrewrite

import vct.col.ast._

import scala.collection.mutable.ArrayBuffer
import RewriteHelpers._
import vct.col.newrewrite.FilterSpecIgnore.{DanglingIgnoreStart, ExtraIgnoreEnd}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object FilterSpecIgnore extends RewriterBuilder {
  override def key: String = "specIgnore"
  override def desc: String = "Remove elements that are ignored with spec_ignore."

  case class DanglingIgnoreStart(start: SpecIgnoreStart[_]) extends UserError {
    override def code: String = "danglingSpecIgnore"
    override def text: String =
      start.o.messageInContext("This spec_ignore is not closed")
  }

  case class ExtraIgnoreEnd(end: SpecIgnoreEnd[_]) extends UserError {
    override def code: String = "extraSpecIgnoreEnd"
    override def text: String =
      end.o.messageInContext("This spec_ignore was not opened")
  }
}

case class FilterSpecIgnore[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case block@Block(statements) =>
      var opens: Seq[SpecIgnoreStart[_]] = Nil
      val result = ArrayBuffer[Statement[Post]]()

      statements.foreach {
        case start: SpecIgnoreStart[_] =>
          opens :+= start
        case end: SpecIgnoreEnd[_] =>
          opens match {
            case Nil => throw ExtraIgnoreEnd(end)
            case _ => opens = opens.init
          }
        case other =>
          if(opens.isEmpty) {
            result += rewriteDefault(other)
          }
      }

      opens match {
        case Nil =>
        case some => throw DanglingIgnoreStart(some.last)
      }

      block.rewrite(result.toSeq)
    case other => rewriteDefault(other)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: ContractApplicable[Pre] =>
      app.contract.requires match {
        case UnitAccountedPredicate(BooleanValue(false)) => app.drop()
        case _ => rewriteDefault(decl)
      }
    case other => rewriteDefault(other)
  }
}
