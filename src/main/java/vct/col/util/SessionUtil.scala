package vct.col.util

import vct.col.ast.`type`.ClassType
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelRegion}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.ASTFactory
import vct.col.util.SessionUtil.getArgName

import scala.sys.error

object SessionUtil {

  val mainClassName : String = "Main"
  val runMethodName : String = "run"
  val mainMethodName : String = "main"
  private val threadName : String = "Thread"
  private val chanName : String = "Chan"
  val channelClassName : String = "Channel"
  val barrierClassName : String = "Barrier"
  val barrierFieldName : String = "threadBarrier"
  val barrierAwait : String = "await"
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

  def getBarrierClass() = new ClassType(barrierClassName)

  def getArgName(name : String) = name + "Arg"

  def getChansFromBlockStateMent(block : ASTNode) : Set[MethodInvokation] = {
    block match {
      case b : BlockStatement =>
        val stats = b.getStatements.toSet : Set[ASTNode]
        stats.flatMap(s => s match {
          case l: LoopStatement => getChansFromBlockStateMent(l.getBody)
          case i : IfStatement => getChansFromBlockStateMent(i.getStatement(0)) ++ (if(i.getCount > 1) getChansFromBlockStateMent(i.getStatement(1)) else Set.empty)
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
      case _ => None
    }
  }

  def getNamesFromExpression(e : ASTNode) : Set[NameExpression] = {
    getNameFromNode(e) match {
      case Some(n) => Set(n)
      case None =>  e match {
        case o : OperatorExpression => o.args.flatMap(getNamesFromExpression).toSet
        case m : MethodInvokation => {
          val objName = getNameFromNode(m.`object`)
          val argsNames = m.getArgs.flatMap(getNamesFromExpression).toSet
          if(objName.isEmpty) argsNames else argsNames + objName.get
        }
        case _ => Set.empty
      }
    }
  }

  def mapInsertSetValue[K,V](key : K, value : V, map : Map[K,Set[V]]) : Map[K,Set[V]] =
    map get key match {
      case None => map + (key -> Set(value))
      case Some(vSet) => map + (key -> (vSet + value))
    }

  def toLineString(s : ASTNode) = s.toString.replace(";\n","")

}

