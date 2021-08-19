package vct.col.ast.generic

import scala.collection.Iterable

trait DebugNode {
  // Assumed to be side-effect free, so no parentheses.
  def debugTreeChildrenFields: Iterable[String]
  def debugTreePropertyFields: Iterable[String]

  def dump(): Unit = {
    DebugSession().dumpNode(0, this)
  }
}
