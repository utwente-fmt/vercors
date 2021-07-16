package vct.col.ast.stmt.composite

import hre.ast.MessageOrigin
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.generic.{ASTNode, DebugNode}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.collection.mutable.ArrayBuffer

object IfStatement {
  val elseGuard = new ConstantExpression(true, new MessageOrigin("else guard"))
}

case class IfStatementCase(var guard: ASTNode, var effect: ASTNode) extends DebugNode {
  override def debugTreeChildrenFields: Iterable[String] = Seq("guard", "effect")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}

class IfStatement extends ASTNode with VisitorHelper {
  private[this] val cases = new ArrayBuffer[IfStatementCase]()

  def getCount = cases.size

  def getGuard(i: Int) = cases.apply(i).guard

  def getStatement(i: Int) = cases.apply(i).effect

  def this(cond: ASTNode, truebranch: ASTNode, falsebranch: ASTNode) = {
    this()
    addClause(cond, truebranch)
    if (falsebranch != null) addClause(IfStatement.elseGuard, falsebranch)
  }

  def addClause(guard: ASTNode, stmt: ASTNode): Unit = {
    val body: ASTNode = if (stmt == null) new BlockStatement() else stmt;
    body.setParent(this)
    if (guard != IfStatement.elseGuard) guard.setParent(this)
    cases += new IfStatementCase(guard, body)
  }

  def hasElse: Boolean = cases.exists(ifCase => ifCase.guard == IfStatement.elseGuard)

  override def accept_simple[T, A](m: ASTMapping1[T, A], arg: A) = m.map(this, arg)

  override def accept_simple[T](v: ASTVisitor[T]) = handle_standard(() => v.visit(this))

  override def accept_simple[T](m: ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("cases")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}
