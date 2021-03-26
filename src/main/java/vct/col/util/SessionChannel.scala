package vct.col.util

import vct.col.ast.expr.{NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTFactory
import vct.col.util.SessionUtil.getArgName

class SessionChannel(val channel: String, val isWrite : Boolean) {

  override def equals(obj: Any): Boolean = obj match {
    case other : SessionChannel => channel == other.channel && isWrite == other.isWrite
    case _ => false
  }

  override def toString: String = channel + " " + (if(isWrite) "Write" else "Read")

  def getArgChanName() : String = getArgName(channel)

  def getArgChan() : SessionChannel = new SessionChannel(getArgChanName(), isWrite)

  def getChanFieldPerm(create : ASTFactory[_]) : ASTNode = {
    val arg1 = create.dereference(create.name(NameExpressionKind.Unresolved,null,channel), if(isWrite) "sent" else "recvd")
    create.expression(StandardOperator.Perm,arg1,getHalfFraction(create))
  }

  def getHalfFraction(create : ASTFactory[_]) = create.expression(StandardOperator.Div,create.constant(1),create.constant(2))
}
