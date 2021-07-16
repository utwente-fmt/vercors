package vct.col.ast.generic

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

class ASTList extends ASTSequence[ASTList] {
  private[this] val list = new ArrayBuffer[ASTNode]()

  override def add(item: ASTNode) = add(Option(item))

  def add(item: Option[ASTNode]) = item match {
    case Some(node) => list += node; this
    case None => this
  }

  override def iterator = list.iterator.asJava

  override def get(i: Int) = list.apply(i)

  override def size = list.size
}
