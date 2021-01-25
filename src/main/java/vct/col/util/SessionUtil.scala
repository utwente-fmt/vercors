package vct.col.util

import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelRegion}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.ASTFactory

import scala.sys.error

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

  def getRoleName(name : String) : String = {
    if(isThreadClassName(name)) {
      name.substring(0,name.length - threadName.length).toLowerCase()
    } else {
      error("Session Fail: Cannot get role from non-Thread-name")
    }
  }

  def isChanName(chan : String) = chan.endsWith(chanName)

  def getChanName(roleName : String) = roleName + chanName

  def getChanClass() = new ClassType(channelClassName)

  def getChansFromBlockStateMent(block : ASTNode) : Set[MethodInvokation] = {
    block match {
      case b : BlockStatement =>
        val stats = b.getStatements.toSet : Set[ASTNode]
        stats.flatMap(s => s match {
          case l: LoopStatement => getChansFromBlockStateMent(l.getBody)
          case p: ParallelRegion => {
            val blocks = p.blocks.map(_.block).toSet: Set[BlockStatement]
            blocks.flatMap(getChansFromBlockStateMent): Set[MethodInvokation]
          }
          case a: AssignmentStatement => getChanFromMethodInvokation(a.expression)
          case o: ASTNode => getChanFromMethodInvokation(o)
        })
      case _ => error("Session Fail: expected BlockStatement"); Set()
    }
  }

  private def getChanFromMethodInvokation(o : ASTNode) : Set[MethodInvokation] = {
    o match {
      case m: MethodInvokation => {
        if (m.method == chanRead || m.method == chanWrite) {
          Set(m)
        } else {
          Set() : Set[MethodInvokation]
        }
      }
      case _ => Set() : Set[MethodInvokation]
    }
  }

  def getNameFromNode(n : ASTNode) : Option[NameExpression] = {
    n match {
      case d : Dereference => d.obj match {
        case n : NameExpression => Some(n)
        case _ => None
      }
      case n : NameExpression => Some(n)
      case m : MethodInvokation => getNameFromNode(m.`object`)
      case _ => None
    }
  }

  def getNamesFromExpression(e : ASTNode) : List[NameExpression] = {
    getNameFromNode(e) match {
      case Some(n) => List(n)
      case None =>  e match {
        case o : OperatorExpression => o.args.flatMap(getNamesFromExpression)
        case _ => List()
      }
    }
  }

}

  class SessionChannel(val channel: String, val isWrite : Boolean) {

    override def equals(obj: Any): Boolean = obj match {
      case other : SessionChannel => channel == other.channel && isWrite == other.isWrite
      case _ => false
    }

    override def toString: String = channel + " " + (if(isWrite) "Write" else "Read")

    def getArgChanName() : String = channel + "Arg"

    def getArgChan() : SessionChannel = new SessionChannel(getArgChanName(), isWrite)

    def getChanFieldPerm(create : ASTFactory[_]) : ASTNode = {
      val arg1 = create.dereference(create.name(NameExpressionKind.Unresolved,null,channel), if(isWrite) "sent" else "recvd")
      create.expression(StandardOperator.Perm,arg1,getHalfFraction(create))
    }

    def getHalfFraction(create : ASTFactory[_]) = create.expression(StandardOperator.Div,create.constant(1),create.constant(2))
}