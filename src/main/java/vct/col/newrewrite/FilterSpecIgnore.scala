package vct.col.newrewrite

import vct.col.ast._

import scala.collection.mutable.ArrayBuffer
import RewriteHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.UserError

case object FilterSpecIgnore extends RewriterBuilder

case class FilterSpecIgnore[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case block@Block(statements) =>
      var level = 0
      val result = ArrayBuffer[Statement[Post]]()

      statements.foreach {
        case SpecIgnoreStart() =>
          level += 1
        case SpecIgnoreEnd() =>
          level -= 1
        case other =>
          if(level == 0) {
            result += rewriteDefault(other)
          }
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
