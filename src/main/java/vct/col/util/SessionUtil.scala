package vct.col.util

import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.{NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTFactory

object SessionUtil {

  val mainClassName : String = "Main"
  val runMethodName : String = "run"
  private val threadName : String = "Thread"
  private val chanName : String = "Chan"
  val channelClassName : String = "Channel"
  val chanWrite : String = "writeValue"
  val chanRead : String = "readValue"

  def getThreadClassName(roleName : String) : String = roleName.toUpperCase() + threadName

  def isThreadClassName(className : String) = className.endsWith(threadName)

  def isChanName(chan : String) = chan.endsWith(chanName)

  def getChanName(roleName : String) = roleName + chanName

  def getChanClass() = new ClassType(channelClassName)

  }

  class SessionChannel(val channel: String, val isWrite : Boolean) {

    override def equals(obj: Any): Boolean = obj match {
      case other : SessionChannel => channel == other.channel && isWrite == other.isWrite
      case _ => false
    }

    def getArgChanName() : String = channel + "Arg"

    def getArgChan() : SessionChannel = new SessionChannel(getArgChanName(), isWrite)

    def getChanFieldPerm(create : ASTFactory[_]) : ASTNode = {
      val arg1 = create.dereference(create.name(NameExpressionKind.Unresolved,null,channel), if(isWrite) "sent" else "recvd")
      create.expression(StandardOperator.Perm,arg1,getHalfFraction(create))
    }

    def getHalfFraction(create : ASTFactory[_]) = create.expression(StandardOperator.Div,create.constant(1),create.constant(2))
}