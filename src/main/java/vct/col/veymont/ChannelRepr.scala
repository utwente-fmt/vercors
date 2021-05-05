package vct.col.veymont

import vct.col.ast.`type`.{ClassType, Type}
import vct.col.ast.expr.{NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTFactory
import Util.{channelClassName, getArgName}

class ChannelRepr(val channel: String, val isWrite : Boolean, val chanType : Type) {

  override def equals(that: Any): Boolean = that match {
    case s : ChannelRepr => s.channel == channel
    case _ => false
  }

  override def hashCode(): Int = channel.hashCode()

  override def toString: String = channel + " " + (if(isWrite) "Write" else "Read")


  def getArgChanName() : String = getArgName(channel)

  def getArgChan() : ChannelRepr = new ChannelRepr(getArgChanName(), isWrite, chanType)

  def getChanFieldPerm(create : ASTFactory[_]) : ASTNode = {
    val arg1 = create.dereference(create.name(NameExpressionKind.Unresolved,null,channel), if(isWrite) "sent" else "recvd")
    create.expression(StandardOperator.Perm,arg1,create.expression(StandardOperator.Div,create.constant(2),create.constant(3)))
  }

  def getChanClass() = new ClassType(chanType.toString + channelClassName)


}
