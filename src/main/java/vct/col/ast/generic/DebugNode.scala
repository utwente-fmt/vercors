package vct.col.ast.generic

import scala.collection.Iterable

trait DebugNode {
  def debugTreeChildrenFields: Iterable[String]
  def debugTreePropertyFields: Iterable[String]

  def dump(): Unit = {
    DebugSession().dumpNode(0, this)
  }
}
