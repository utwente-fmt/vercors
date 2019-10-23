package vct.col.ast.generic

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class ASTList extends ASTSequence[ASTList] {
  private[this] val list = new ArrayBuffer[ASTNode]()
  
  def add(item:Option[ASTNode]) = item match {
    case Some(node) => list += node; this
    case None => this
  }
  
  override def add(item:ASTNode) = add(Option(item))
  override def iterator = list.toIterator.asJava
  override def get(i:Int) = list.apply(i)
  override def size = list.size
}
