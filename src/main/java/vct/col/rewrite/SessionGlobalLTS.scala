package vct.col.rewrite

import vct.col.ast.expr.{Dereference, NameExpression, OperatorExpression}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement}
import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{getNameFromNode, getNamesFromExpression, mapInsertSetValue, runMethodName}

import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}
import java.io.PrintWriter

sealed trait GlobalAction
sealed trait LocalAction

case object BarrierWait extends LocalAction with GlobalAction
case object ErrorAction extends LocalAction with GlobalAction
final case class LocalAssign(assign : AssignmentStatement) extends LocalAction with GlobalAction
final case class CommunicationAction(receiverWithField : ASTNode, sender : NameExpression, sendExpression : ASTNode) extends GlobalAction

final case class ReadAction(receiverWithField : ASTNode, sender : NameExpression) extends LocalAction
final case class WriteAction(receiver : NameExpression, sender : String, sendExpression : ASTNode) extends LocalAction
case object Tau extends LocalAction

final class LTSState(val method : Method, val nextStatements : Array[ASTNode])
final class LTSLabel(val condition: Option[OperatorExpression], val action : GlobalAction)

class SessionGlobalLTS(override val source : ProgramUnit, val out : PrintWriter) extends RecursiveVisitor(source, true) {

  private var states = Set[LTSState]()
  private var initialState : LTSState = null
  private var transitions : Map[LTSState,Set[(LTSLabel,LTSState)]] = Map()
  private var currentState : LTSState = null
  private val roleNames = SessionStructureCheck.getRoleNames(source)

  def generateLTSAndPrint() : Unit = {
    generateLTS()
  }

  private def generateLTS() : Unit = {
    val mainClass = SessionStructureCheck.getMainClass(source)
    val runMethod = mainClass.methods().find(_.name == runMethodName).get
    initialState = new LTSState(runMethod,runMethod.getBody.asInstanceOf[BlockStatement].getStatements)
    states += initialState
    currentState = initialState
    visit(runMethod)
  }

  override def visit(m : Method) : Unit = m.getBody.accept(this)

  override def visit(b : BlockStatement) : Unit = visitStatementSequence(b.getStatements)

  def visitStatementSequence(seq : Array[ASTNode]) : Unit = {
    if(seq.size > 0) {
      val s1 = seq.head
      val tmpCurrent = currentState //store state before executing s1
      val nextSeqStrong = seq.tail.clone()
      visitStatement(s1,nextSeqStrong)
      visitStatementSequence(nextSeqStrong)
      if (seq.size > 1) {
        val s2 = seq.tail.head
        if (!isIfOrWhile(s2) && weakSequenceAllowed(s1,s2)) { //if weak sequence allowed: also do other order s2;s1
          currentState = tmpCurrent
          val nextSeqWeak = s1 +: seq.tail.tail.clone()
          visitStatement(s2,nextSeqWeak)
          visitStatementSequence(nextSeqWeak)
        }
      }
    }
  }

  def visitStatement(s : ASTNode, nextSeq : Array[ASTNode]) : Unit = {
    s.accept(this)
    currentState = new LTSState(currentState.method,nextSeq)
    states += currentState
  }

  def weakSequenceAllowed(s1 : ASTNode, s2: ASTNode) : Boolean

  def isIfOrWhile(n : ASTNode) : Boolean = n match {
    case _ : IfStatement => true
    case _ : LoopStatement => true
    case _ => false
  }

  override def visit(a : AssignmentStatement) : Unit = {
    val label = new LTSLabel(None,getGlobalAction(a))
    transitions += mapInsertSetValue(currentState,label,transitions)
  }

  def getGlobalAction(a : AssignmentStatement) : GlobalAction =
    getNameFromNode(a.location) match {
      case None => ErrorAction
      case Some(locRole) => {
        val expRole = getNamesFromExpression(a.expression)
        if(expRole.isEmpty || expRole.size == 1 && expRole.head.name == locRole.name)
          LocalAssign(a)
        else if(expRole.size == 1 && expRole.head.name != locRole.name)
          CommunicationAction(a.location,expRole.head,a.expression)
        else ErrorAction
      }
    }


}
