package vct.col.ast

import hre.ast.MessageOrigin;
import vct.col.util.VisitorHelper

object IfStatement {
  def elseGuard = new ConstantExpression(true, new MessageOrigin("else guard"))
}

class IfStatement extends ASTNode with VisitorHelper  {
  private[this] val cases = new java.util.ArrayList[(ASTNode,ASTNode)]()
  
  def getCount = cases.size
  def getGuard(i:Int) = cases.get(i)._1
  def getStatement(i:Int) = cases.get(i)._2
  
  def addClause(guard:ASTNode, stmt:ASTNode) : Unit = {
    stmt.setParent(this)
    if (guard != IfStatement.elseGuard) {
      guard.setParent(this)
    }
    cases.add(new Tuple2(guard, stmt))
  }
  
  def this(cond:ASTNode, truebranch:ASTNode, falsebranch:ASTNode) = {
    this()
    addClause(cond, truebranch)
    if (falsebranch != null) {
      addClause(IfStatement.elseGuard, falsebranch)
    }
  }
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}
