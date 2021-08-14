package vct.col.ast.stmt.composite

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/*This method is explicitly provide because Intellij has a bug regarding resolving methods of parameters in Scala classes */
trait IntellijExplicitGetter {
  def main():BlockStatement;
}

class TryWithResources(private[this] val resources: ArrayBuffer[DeclarationStatement],
                       val main: BlockStatement,
                       val after: Option[BlockStatement],
                       private[this] val catchClauses: ArrayBuffer[CatchClause])
extends ASTNode with IntellijExplicitGetter
{
  def this(main: BlockStatement, after: BlockStatement) =
    this(ArrayBuffer(), main, Option(after), ArrayBuffer())

  def clauses = catchClauses.asJava
  def javaResources = resources.asJava

  def addCatchClause(name: String, catchTypes: Seq[Type], block: BlockStatement) : Unit =
    catchClauses += new CatchClause(name, catchTypes, block)

  def addCatchClauseArray(name: String, catchTypes: Array[Type], block: BlockStatement): Unit =
    addCatchClause(name, catchTypes.toSeq, block)

  def addResource(decl: DeclarationStatement): Unit =
    resources += decl

  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = visitor.visit(this)
  override def accept_simple[T](map: ASTMapping[T]): T = map.map(this)
  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = map.map(this, arg)

  override def debugTreeChildrenFields: Iterable[String] = Seq("resources", "main", "after", "catchClauses")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
