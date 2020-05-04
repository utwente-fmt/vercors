package vct.col.ast.stmt.composite

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

class TryWithResources(private[this] val resources: ArrayBuffer[DeclarationStatement],
                       val main: BlockStatement,
                       val after: Option[BlockStatement],
                       private[this] val catchClauses: ArrayBuffer[CatchClause])
extends ASTNode
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
