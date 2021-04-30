package vct.col.newrewrite

import vct.col.ast._

import scala.collection.mutable.ArrayBuffer
import RewriteHelpers._

case class FilterSpecIgnore() extends Rewriter {
  override def dispatch(stat: Statement): Statement = stat match {
    case block@Block(statements) =>
      var level = 0
      val result = ArrayBuffer[Statement]()

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

  override def dispatch(decl: Declaration): Unit = decl match {
    case app: ContractApplicable =>
      app.contract.requires match {
        case Constant(false) => // drop declaration
        case _ => rewriteDefault(decl)
      }
    case other => rewriteDefault(other)
  }
}
