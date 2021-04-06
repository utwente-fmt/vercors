package vct.col.rewrite

import hre.config.StringSetting
import hre.lang.System.{Debug, Fail}
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.print.PVLPrinter
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.syntax.PVLSyntax
import vct.col.ast.util.{ASTFactory, AbstractRewriter, Configuration, RecursiveVisitor}
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{getNameFromNode, getNamesFromExpression, mapInsertSetValue, runMethodName, toLineString}

import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}
import java.io.{File, FileOutputStream, IOException, PrintWriter}
import java.util
import scala.collection.mutable.ListBuffer

sealed trait GlobalAction
sealed trait LocalAction

case object BarrierWait extends LocalAction with GlobalAction {
  override def toString: String = "BarrierWait"
}
case object ErrorAction extends LocalAction with GlobalAction
final case class LocalAssign(assign : AssignmentStatement) extends LocalAction with GlobalAction {
  override def toString: String = assign.toString
}
final case class CommunicationAction(receiver : NameExpression, receiverField : String, sender : NameExpression, sendExpression : ASTNode) extends GlobalAction {
  override def toString: String = {
    toLineString(sendExpression) + " --|> " + receiver + "." + receiverField
  }
}

final case class ReadAction(receiverWithField : ASTNode, sender : NameExpression) extends LocalAction
final case class WriteAction(receiver : NameExpression, sender : String, sendExpression : ASTNode) extends LocalAction
case object Tau extends LocalAction with GlobalAction

final class LTSState(val nextStatements : List[ASTNode]) {
  override def equals(that: Any): Boolean = that match {
    case s : LTSState => this.toString == s.toString()
    case _ => false
  }
  override def hashCode(): Int = this.toString().hashCode()

  override def toString: String = nextStatements.map(PVLSyntax.get().print(_).toString).toString()

  def getCopy(copy_rw : AbstractRewriter) = new LTSState(copy_rw.rewrite(nextStatements.toArray).toList)
}
final class LTSLabel(val condition: Option[ASTNode], val action : GlobalAction) {
  override def toString: String = "(" + (condition match { case None => "true"; case Some(c) => c.toString}) + " , " + action.toString + ")"
}
final class LTSTransition(val label : LTSLabel, val destState : LTSState) {
  override def toString: String = label.toString + " -> " + destState.toString
}

class SessionGlobalLTS(override val source : ProgramUnit) extends AbstractRewriter(null, true){

  private var initialState : LTSState = null
  private var transitions : Map[LTSState,Set[LTSTransition]] = Map()
  private var currentState : LTSState = null
  private val roleNames = SessionStructureCheck.getRoleNames(source)

  val session_global_lts = new StringSetting("examples/private/globalLTS.txt")
  val session_local_lts = new StringSetting("examples/private/localLTS.txt")

  def generateLTSAndPrint() : Unit = {
    generateLTS()
    print()
  }

  private def print(out : PrintWriter) : Unit = {
    val destStates : Set[LTSState] = transitions.values.flatMap(_.map(_.destState)).toSet
    val states : Set[LTSState] = transitions.keys.toSet ++ destStates
    val stateMap : Map[LTSState,Int] = states.zipWithIndex.toMap
    val nrTrans = transitions.values.map(_.size).sum
    out.println("des " + stateMap(initialState) + " " + nrTrans + " " + states.size)
    transitions foreach { case (src, trset) =>
      trset.foreach(tr => {
        out.println(stateMap(src) + " -> " + tr.label + " -> " + stateMap(tr.destState))
      })
    }
    out.println("\nStates:")
    stateMap.foreach{ case (s,i) => out.println(i + ": " + s)}
  }

  private def print() : Unit = {
    try {
      val f = new File(session_global_lts.get());
      val b = f.createNewFile();
      if (!b) {
        Debug("File %s already exists and is now overwritten", session_global_lts.get());
      }
      val out = new PrintWriter(new FileOutputStream(f));
      print(out)
      out.close();
    } catch {
      case e: IOException => Debug(e.getMessage);
    }
  }

  private def generateLTS() : Unit = {
    val mainClass = SessionStructureCheck.getMainClass(source)
    val runMethod = mainClass.methods().find(_.name == runMethodName).get
    initialState = new LTSState(runMethod.getBody.asInstanceOf[BlockStatement].getStatements.toList).getCopy(copy_rw)
    currentState = initialState
    val runBody = runMethod.getBody.asInstanceOf[BlockStatement]
    visitStatementSequence(copy_rw.rewrite(runBody.getStatements).toList,false)
  }

  override def visit(m : Method) : Unit =
    Fail("Session Fail: method visit(Method) should not be reached")

  override def visit(b : BlockStatement) : Unit =
    Fail("Session Fail: method visit(BlockStatement) should not be reached")

  def visitStatementSequence(seq : List[ASTNode], extendCurrentStateTrans : Boolean) : Unit = {
    if((extendCurrentStateTrans || !transitions.contains(currentState)) && seq.nonEmpty) {
      val tmpCurrent = currentState.getCopy(copy_rw) //store state before executing s1
      val s1 = seq.head
      val nextSeq = seq.tail
      visitNode(s1, nextSeq)
      if(nextSeq.nonEmpty) {
        val s2 = seq.tail.head
        if (!isIfOrWhile(s2) && weakSequenceAllowed(s1, s2)) { //if weak sequence allowed: also do other order s2;s1
          currentState = tmpCurrent
          val nextSeq2 = s1 +: currentState.nextStatements.tail.tail
          visitNode(s2, nextSeq2)
        }
      }
    }
  }

  def visitNode(n : ASTNode, seqAfterFirstStatement : List[ASTNode]) : Unit = n match {
    case a : AssignmentStatement => visit(a,seqAfterFirstStatement)
    case i : IfStatement => visit(i,seqAfterFirstStatement)
    case l : LoopStatement => visit(l, seqAfterFirstStatement)
    case p : ParallelRegion => visit(p,seqAfterFirstStatement)
    case m : MethodInvokation => visit(m, seqAfterFirstStatement)
    case _ => Fail("Session Fail: cannot visit this type of statement!")
  }

  def takeTransition(label : LTSLabel, nextStateSeq : List[ASTNode]) : Unit = {
    val nextState = new LTSState(nextStateSeq)
    transitions = mapInsertTransition(currentState, new LTSTransition(label,nextState))
    currentState = nextState
  }

  def weakSequenceAllowed(s1 : ASTNode, s2: ASTNode) : Boolean = {
    getSubjects(Set.empty,s1).intersect(getSubjects(Set.empty,s2)).isEmpty
  }

  def getSubjects(seen : Set[String],s : ASTNode) : Set[String] = {
    s match {
      case b : BlockStatement => b.getStatements.toSet.flatMap(getSubjects(seen,_))
      case a : AssignmentStatement => getGlobalAction(a) match {
        case LocalAssign(assign) => Set(assign.location.asInstanceOf[Dereference].obj.asInstanceOf[NameExpression].name)
        case CommunicationAction(receiver,_, sender, _) =>
          Set(receiver.name, sender.name)
        case _ => Set.empty
      }
      case i : IfStatement => roleNames.toSet
      case l : LoopStatement => roleNames.toSet
      case p : ParallelRegion => p.blocks.map(_.block).toSet.flatMap(getSubjects(seen,_))
      case m : MethodInvokation => getNamesFromExpression(m).map(_.name).toSet  ++ (if(seen.contains(m.method)) Set.empty else getSubjects(seen + m.method,m.definition.getBody))
    }
  }

  def isIfOrWhile(n : ASTNode) : Boolean = n match {
    case _ : IfStatement => true
    case _ : LoopStatement => true
    case _ => false
  }

  def mapInsertTransition(k : LTSState, v : LTSTransition) : Map[LTSState,Set[LTSTransition]] =
    mapInsertSetValue[LTSState,LTSTransition](k,v,transitions)

  def visit(a : AssignmentStatement, seq : List[ASTNode]) = {
    takeTransition(new LTSLabel(None,getGlobalAction(a)), seq)
    visitStatementSequence(seq,false)
  }

  override def visit(a : AssignmentStatement) : Unit = {
    Fail("Session Fail: method visit(AssignmentStatement) should not be reached")
    //takeTransition(new LTSLabel(None,getGlobalAction(a)))
  }

  def getGlobalAction(a : AssignmentStatement) : GlobalAction =
    getNameFromNode(a.location) match {
      case None => ErrorAction
      case Some(locRole) => {
        val expRole = getNamesFromExpression(a.expression)
        if(expRole.isEmpty || expRole.size == 1 && expRole.head.name == locRole.name)
          LocalAssign(a)
        else if(expRole.size == 1 && expRole.head.name != locRole.name)
          CommunicationAction(locRole,a.location.asInstanceOf[Dereference].field,expRole.head,a.expression)
        else ErrorAction
      }
    }

  def visit(i: IfStatement, seq : List[ASTNode]) : Unit =
    takeTwoBranches(i.getGuard(0),ASTNodeToList(i.getStatement(0)),ASTNodeToList(i.getStatement(1)),seq)

  override def visit(i: IfStatement) =
    Fail("Session Fail: method visit(IfStatement) should not be reached")

  def visit(l : LoopStatement, seq : List[ASTNode]) : Unit =
    takeTwoBranches(l.getEntryGuard,ASTNodeToList(l.getBody) :+ l,List.empty,seq)

  override def visit(l : LoopStatement) =
    Fail("Session Fail: method visit(LoopStatement) should not be reached")

  private def ASTNodeToList(n : ASTNode) : List[ASTNode] = n match {
    case b : BlockStatement => b.getStatements.toList
    case a : ASTNode => List(a)
  }

  //visit(new IfStatement(l.getEntryGuard,create.block(l.getBody,l),new BlockStatement()))

  private def takeTwoBranches(cond : ASTNode, leftBranch : List[ASTNode], rightBranch : List[ASTNode], seq : List[ASTNode]) : Unit = {
    val tmpCurrent = currentState.getCopy(copy_rw)
    val condTrue = new LTSLabel(Some(cond),BarrierWait)
    takeIfBranch(condTrue,leftBranch,seq)
    currentState = tmpCurrent
    val condFalse = new LTSLabel(Some(OperatorExpression(StandardOperator.Not,List(cond))),BarrierWait)
    takeIfBranch(condFalse,rightBranch,seq)
  }

  private def takeIfBranch(condLabel : LTSLabel, statements : List[ASTNode], seq : List[ASTNode]) : Unit = {
    val totalNextSeq = statements ++ seq
    takeTransition(condLabel, totalNextSeq)
    visitStatementSequence(totalNextSeq,false)
  }

  override def visit(pr : ParallelRegion) : Unit =
    Fail("Session Fail: method visit(ParallelRegion) should not be reached")

  def visit(pr : ParallelRegion, nextSeq : List[ASTNode]) : Unit = {
    val copiedPR = create.region(pr.contract,pr.blocks.map(getCopyParBlock):_*)
    val tmpCurrent = new LTSState(copiedPR +: nextSeq)
    val nextSequences = pr.blocks.indices.map(i => {
      val split = pr.blocks.splitAt(i)
      (split._2.head, split._1 ++ split._2.tail)
    }).map{ case (b,others) => {
      val afterAction = getTailParBlock(b)
      val newPr = create.region(pr.contract,(if (afterAction.block.isEmpty) others else afterAction +: others):_*)
      if(newPr.blocks.forall(_.block.size == 0))
        b.block.getStatement(0) +: nextSeq
      else
        b.block.getStatement(0) +: newPr +: nextSeq
    }}
    nextSequences.foreach(seq => {
      currentState = tmpCurrent
      visitStatementSequence(seq,true)
    })
    /*
        for(i <- pr.blocks.indices) {
          currentState = tmpCurrent
          nextSequence = tmpSeq
          val b = pr.blocks(i)
          if(b.block.size > 0) {
            val otherBlocks = pr.blocks.take(i) ++ pr.blocks.drop(i+1)
            val newBlocks = getTailParBlock(b) +: otherBlocks
            val newPr = create.region(pr.contract,newBlocks:_*)
            if(newPr.blocks.forall(_.block.size == 0))
              nextSequence = b.block.getStatement(0) +: nextSequence
            else
              nextSequence = b.block.getStatement(0) +: newPr +: nextSequence
            visitStatementSequence()
          }
        } */
  }

  def getCopyBlockWithStats(pb : ParallelBlock, statements : Array[ASTNode]) : ParallelBlock =
    create.parallel_block(pb.label,pb.contract,pb.itersJava,create.block(copy_rw.rewrite(statements):_*), pb.deps)

  def getTailParBlock(pb : ParallelBlock) : ParallelBlock = getCopyBlockWithStats(pb,pb.block.getStatements.tail)

  def getCopyParBlock(pb : ParallelBlock) : ParallelBlock = getCopyBlockWithStats(pb, pb.block.getStatements)

  override def visit(m : MethodInvokation) : Unit =
    Fail("Session Fail: method visit(MethodInvokation) should not be reached")

  def visit(m : MethodInvokation, nextSeq : List[ASTNode]) = {
    if(m.`object` == null) { // it is a main method
      visitStatementSequence(ASTNodeToList(m.definition.getBody) ++ nextSeq, false)
    } else {//role method
      takeTransition(new LTSLabel(None, Tau),nextSeq)
      visitStatementSequence(nextSeq,false)
    }
  }


}
