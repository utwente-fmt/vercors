package vct.col.veymont

import hre.config.Configuration
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite._
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.syntax.PVLSyntax
import vct.col.ast.util.AbstractRewriter
import Util._
import geny.Generator.from
import hre.lang.System.Output
import vct.col.veymont.StructureCheck.isExecutableMainMethod

import java.io.{File, FileOutputStream, IOException, PrintWriter}
import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.jdk.CollectionConverters._

sealed trait Action
sealed trait GlobalAction extends Action
sealed trait LocalAction extends Action

case object BarrierWait extends LocalAction with GlobalAction {
  override def toString: String = "BarrierWait"
}
case object ErrorAction extends LocalAction with GlobalAction
final case class SingleRoleAction(n : ASTNode) extends LocalAction with GlobalAction {
  override def toString: String = n.toString
}
final case class CommunicationAction(receiver : NameExpression, receiverField : String, sender : NameExpression, sendExpression : ASTNode) extends GlobalAction {
  override def toString: String = {
    toLineString(sendExpression) + " Comm " + receiver + "." + receiverField
  }
}

final case class ReadAction(receiver : NameExpression, sender : NameExpression, receiveExpression : ASTNode) extends LocalAction {
  override def toString: String = sender + " " + receiver + " Read " + toLineString(receiveExpression)
}
final case class WriteAction(receiver : NameExpression, sender : String, sendExpression : ASTNode) extends LocalAction {
  override def toString: String = sender + " " + receiver + " Write " + toLineString(sendExpression)
}
case object Tau extends LocalAction {
  override def toString: String = "Tau"
}

final class LTSState(val nextStatements : List[ASTNode]) {
  override def equals(that: Any): Boolean = that match {
    case s : LTSState => this.toString == s.toString
    case _ => false
  }
  override def hashCode(): Int = this.toString.hashCode()

  override def toString: String = nextStatements.map(PVLSyntax.get().print(_).toString).toString

  def getCopy(copy_rw : AbstractRewriter) = new LTSState(copy_rw.rewrite(nextStatements.toArray).toList)
}
final class LTSLabel(val condition: Option[ASTNode], val action : Action) {
  override def toString: String = (condition match { case None => "true"; case Some(c) => c.toString}) + " @ " + action.toString
}
final class LTSTransition(val label : LTSLabel, val destState : LTSState) {
  override def toString: String = label.toString + " -> " + destState.toString
}

class GenerateLTS(override val source : ProgramUnit, isGlobal : Boolean) extends AbstractRewriter(null, true){

  private var initialState : LTSState = null
  private var transitions = Map.empty[LTSState,Set[LTSTransition]]
  private var roleNames : Iterable[String] = null
  private var roleName : String = null

  private val veymontFileName = Configuration.veymont_file.get()
  private def veymontGlobalLts : String = {
    require(veymontFileName.endsWith(".pvl"))
    veymontFileName.slice(0,veymontFileName.length-4) + "LTS.aut"
  }
  private def veymontLocalLts : String = {
    require(veymontFileName.endsWith(".pvl"))
    veymontFileName.slice(0,veymontFileName.length-4) + roleName + "LTS.aut"
  }

  def generateLTSAndCheckWellBehavedness() : Unit = {
    if(isGlobal) {
      roleNames = StructureCheck.getRoleNames(source)
      generateLTS(StructureCheck.getMainClass(source))
      print()
    } else {
      val roleClasses = source.get().asScala.filter(n => isThreadClassName(n.name)).map(_.asInstanceOf[ASTClass])
      roleNames = roleClasses.map(c => getRoleName(c.name))
      for(thread <- roleClasses) {
        initialState = null
        transitions = Map.empty
        roleName = thread.fields().asScala.head.name
        generateLTS(thread)
        //print() //use this method to print LTS to a file
        WellBehavednessIterative.check(transitions,roleName)
      }
    }
  }

  private def print(out : PrintWriter) : Unit = {
    val states : Seq[LTSState] = (transitions.keys.toList ++ transitions.values.flatMap(_.map(_.destState))).distinct
    val stateMap : Map[LTSState,Int] = states.zipWithIndex.toMap
    val nrTrans = transitions.values.map(_.size).sum
    out.println("des (" + stateMap(initialState) + "," + nrTrans + "," + states.size + ")")
    transitions foreach { case (src, trset) =>
      trset.foreach(tr => {
        out.println("(" + stateMap(src) + ",\"" + tr.label + "\"," + stateMap(tr.destState) + ")")
      })
    }
  }

  private def print() : Unit = {
    try {
      val f = if(isGlobal) new File(veymontGlobalLts) else new File(veymontLocalLts)
      val dir = f.toPath.getParent.toFile
      if(!dir.isDirectory)
        dir.mkdir()
      val b = f.createNewFile()
      if (!b) {
        Debug("File %s already exists and is now overwritten", f.toString)
      }
      val out = new PrintWriter(new FileOutputStream(f))
      print(out)
      out.close()
    } catch {
      case e: IOException => Debug(e.getMessage)
    }
  }

  private def getStatementsFromNode(classDef : ASTClass, mainMethods : Iterable[Method],a : ASTNode) : List[ASTNode] = a match {
    case m: MethodInvokation =>
      if (m.`object` == null) {
        preProcessMethodCalls(classDef,mainMethods,m.definition)
      } else List(m)
    case n: ASTNode => List(n)
  }

  private def preProcessMethodCalls(classDef : ASTClass,  mainMethods : Iterable[Method], method : Method) : List[ASTNode] = {
    val stats = method.getBody.asInstanceOf[BlockStatement].getStatements.toList
    stats.flatMap(s => getStatementsFromNode(classDef, mainMethods, s))
  }

  private def generateLTS(classDef : ASTClass) : Unit = {
    val runMethod = classDef.methods().asScala.find(_.name == runMethodName).get
    val mainMethods = classDef.methods().asScala.filter(isExecutableMainMethod)
    initialState = new LTSState(preProcessMethodCalls(classDef,mainMethods,runMethod)).getCopy(copy_rw)
    visitStatementSequence(initialState,initialState.nextStatements)
  }

  def getNrLastWeakFirstStatements(seq : List[ASTNode], seen : List[ASTNode]) : Int =
    seq match {
      case Nil => 0
      case (x :: Nil) => 1
      case s1 :: s2 :: xs =>
        if (weakSequenceAllowed(s1, s2) && seen.forall(s0 => weakSequenceAllowed(s0, s2))) {
          1 + getNrLastWeakFirstStatements(s2 :: xs, s1 +: seen)
        } else 1
    }

  private def getElAndRestByIndex[A](seq : List[A], index : Int) : (A, List[A]) = {
    val split = seq.splitAt(index)
    (split._2.head, split._1 ++ split._2.tail)
  }

  private def getWeakSequences(seq : List[ASTNode]) : List[(ASTNode,List[ASTNode])] = {
    val nr = getNrLastWeakFirstStatements(seq,List.empty)
    (for(i <- 0 until nr) yield getElAndRestByIndex(seq,i)).toList
  }

  def visitStatementSequence(currentState : LTSState, seq : List[ASTNode]) : Unit = {
    if(seq.nonEmpty && !transitions.contains(currentState)) {
      visitStatementSequenceAbstract(currentState,getWeakSequences(seq))
    }
  }

  def visitStatementSequenceAbstract(currentState : LTSState, firstAndNext :List[(ASTNode,List[ASTNode])]) : Unit =
    firstAndNext.foreach{case (s,seq) => visitNode(s,currentState,seq)}

  def visitNode(n : ASTNode, currentState : LTSState, seqAfterFirstStatement : List[ASTNode]) : Unit = n match {
    case a : AssignmentStatement => visit(a,currentState,seqAfterFirstStatement)
    case i : IfStatement => visit(i,currentState,seqAfterFirstStatement)
    case l : LoopStatement => visit(l,currentState,seqAfterFirstStatement)
    case p : ParallelRegion => visit(p,currentState,seqAfterFirstStatement)
    case m : MethodInvokation => visit(m,currentState,seqAfterFirstStatement)
    case s : ASTSpecial => visit(s,currentState,seqAfterFirstStatement)
    case _ => Fail("VeyMont Fail: cannot visit this type of statement!")
  }

  def takeTransition(currentState : LTSState, label : LTSLabel, nextStateSeq : List[ASTNode]) : LTSState = {
    val nextState = new LTSState(nextStateSeq)
    Debug(label.action.toString.replace('%','^') + " " + (if(label.action == BarrierWait) label.condition.get else ""))
    transitions = mapInsertTransition(currentState, new LTSTransition(label,nextState))
    nextState
  }

  def weakSequenceAllowed(s1 : ASTNode, s2: ASTNode) : Boolean = {
    if(!canSwap(s1) || !canSwap(s2))
      false
    else getSubjects(Set.empty,s1).intersect(getSubjects(Set.empty,s2)).isEmpty
  }

  def getSubjects(seen : Set[String],s : ASTNode) : Set[String] = {
    s match {
      case b : BlockStatement => b.getStatements.toSet.flatMap(getSubjects(seen,_))
      case a : AssignmentStatement => getGlobalAction(a) match {
        case SingleRoleAction(node) => node match {
          case an : AssignmentStatement => an.location match {
            case d : Dereference => Set(d.obj.asInstanceOf[NameExpression].name)
            case n : NameExpression => Set(n.name)
            case _ => throw Failure("VeyMont Fail: cannot determine subject of  assignment " + a.toString)
          }
          case m : MethodInvokation => m.`object` match {
            case n : NameExpression => Set(n.name)
            case _ => throw Failure("VeyMont Fail: cannot determine subject of  assignment " + a.toString)
          }
        }
        case CommunicationAction(receiver,_, sender, _) =>
          Set(receiver.name, sender.name)
        case _ => Set.empty
      }
      case _ : IfStatement => roleNames.toSet
      case _ : LoopStatement => roleNames.toSet
      case p : ParallelRegion => p.blocks.map(_.block).toSet.flatMap(getSubjects(seen,_))
      case m : MethodInvokation => {
        if(m.`object` == null) { //it is a main method
          Fail("VeyMont Fail: encountered method call %s in LTS generation",m.method)
          Set.empty
        } else getNamesFromExpression(m).map(_.name)
      }
      case _ : ASTSpecial => Set.empty
    }
  }

  def canSwap(n : ASTNode) : Boolean = n match {
    case _ : IfStatement => false
    case _ : LoopStatement => false
    case _ => true
  }

  def visit(s : ASTSpecial, currentState : LTSState, seq : List[ASTNode]) : Unit =
    if (s.kind == ASTSpecial.Kind.TauAction) {
      val nextState = takeTransition(currentState, new LTSLabel(None, Tau), seq)
      visitStatementSequence(nextState, seq)
    } else if(s.kind == ASTSpecial.Kind.Assert) {
      visitStatementSequence(currentState, seq) //skip assert
    } else Fail("VeyMont Fail: cannot visit this type of statement!")


  def mapInsertTransition(k : LTSState, v : LTSTransition) : Map[LTSState,Set[LTSTransition]] =
    mapInsertSetValue[LTSState,LTSTransition](k,v,transitions)

  def visit(a : AssignmentStatement, currentState : LTSState, seq : List[ASTNode]) : Unit = {
    val nextState = takeTransition(currentState, new LTSLabel(None,if(isGlobal) getGlobalAction(a) else getLocalAction(a,roleName)), seq)
    visitStatementSequence(nextState,seq)
  }

  def getGlobalAction(a : AssignmentStatement) : GlobalAction =
    getNameFromNode(a.location) match {
      case None => ErrorAction
      case Some(locRole) => {
        val expRole = getNamesFromExpression(a.expression)
        if(expRole.isEmpty || expRole.size == 1 && expRole.head.name == locRole.name)
          SingleRoleAction(a)
        else if(expRole.size == 1 && expRole.head.name != locRole.name)
          CommunicationAction(locRole,getFieldFromDereference(a.location),expRole.head,a.expression)
        else ErrorAction
      }
    }

  @tailrec
  private def getFieldFromDereference(n : ASTNode) : String = n match {
    case d : Dereference => d.field
    case op : OperatorExpression =>
      if(op.operator == StandardOperator.Subscript)
        getFieldFromDereference(op.arg(0))
      else throw Failure("VeyMont Fail: not an array element!")
    case _ => throw Failure("VeyMont Fail: not a Dereference! %s",n)
  }

  def getLocalAction(a: AssignmentStatement, roleName : String) : LocalAction = {
    val expRole = getNamesFromExpression(a.expression)
    getNameFromNode(a.location) match {
      case Some(locRole) => {
        if(locRole.name == roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name == roleName))
          SingleRoleAction(a)
        else if(locRole.name == roleName && isReadChanMethod(a.expression)) {
          val chan = a.expression.asInstanceOf[MethodInvokation].`object`.asInstanceOf[NameExpression].name
          val sender = chan.take(chan.length - chanName.length - locRole.name.length)
          ReadAction(locRole,create.field_name(sender),a.location)
        } else if(locRole.name != roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name != roleName))
          Tau
        else ErrorAction
      }
      case None => ErrorAction
    }
  }

  private def isReadChanMethod(n : ASTNode) = n match {
    case m : MethodInvokation => m.method == chanReadMethodName
    case _ => false
  }

  def visit(i: IfStatement, currentState : LTSState, seq : List[ASTNode]) : Unit =
    takeTwoBranches(i.getGuard(0),ASTNodeToList(i.getStatement(0)),if(i.getCount > 1) ASTNodeToList(i.getStatement(1)) else List.empty,currentState,seq)

  def isRecursion(s : ASTNode, recursiveNodes : List[ASTNode]) : Boolean =
    recursiveNodes.exists(n => new LTSState(List(n)) == new LTSState(List(s)))

  def visit(l : LoopStatement, currentState : LTSState, seq : List[ASTNode]) : Unit =
      takeTwoBranches(l.getEntryGuard,ASTNodeToList(l.getBody) :+ l,List.empty,currentState,seq)

  private def ASTNodeToList(n : ASTNode) : List[ASTNode] = n match {
    case b : BlockStatement => b.getStatements.toList
    case a : ASTNode => List(a)
  }

  private def takeTwoBranches(cond : ASTNode, leftBranch : List[ASTNode], rightBranch : List[ASTNode], currentState : LTSState, seq : List[ASTNode]) : Unit = {
    val condTrue = new LTSLabel(Some(cond),BarrierWait)
    takeIfBranch(condTrue,leftBranch,currentState,seq)
    val condFalse = new LTSLabel(Some(OperatorExpression(StandardOperator.Not,List(cond))),BarrierWait)
    takeIfBranch(condFalse,rightBranch,currentState,seq)
  }

  private def takeIfBranch(condLabel : LTSLabel, statements : List[ASTNode], currentState : LTSState, seq : List[ASTNode]) : Unit = {
    val totalNextSeq = statements ++ seq
    val nextState = takeTransition(currentState, condLabel, totalNextSeq)
    visitStatementSequence(nextState, totalNextSeq)
  }

  def visit(pr : ParallelRegion, currentState : LTSState, nextSeq : List[ASTNode]) : Unit =
    pr.blocks.indices.foreach(i => {
      val (left,right) = pr.blocks.splitAt(i)
      val b = right.head
      val fns = getWeakSequences(b.block.getStatements.toList).map { case (fnleft, fnright) =>
        (fnleft, getNewPrBeforeNextSeq(b, fnright, left, right.tail, pr, nextSeq))
      }
      visitStatementSequenceAbstract(currentState,fns)
      })

  private def getNewPrBeforeNextSeq(b : ParallelBlock, seq : List[ASTNode], othersPreBlock : List[ParallelBlock], othersPostBlock : List[ParallelBlock], pr : ParallelRegion, nextSeq : List[ASTNode]) : List[ASTNode] = {
    val afterFirstbAction = getCopyBlockWithStats(b,seq)
    val newPr = create.region(pr.contract,(if (afterFirstbAction.block.isEmpty) othersPreBlock ++ othersPostBlock  else othersPreBlock ++ (afterFirstbAction +: othersPostBlock)):_*)
    if(newPr.blocks.forall(_.block.size == 0)) nextSeq else newPr +: nextSeq
  }

  private def getCopyBlockWithStats(pb : ParallelBlock, statements : List[ASTNode]) : ParallelBlock =
    create.parallel_block(pb.label,pb.contract,pb.itersJava,create.block(copy_rw.rewrite(statements.toArray):_*), pb.deps)

  def visit(m : MethodInvokation, currentState : LTSState, nextSeq : List[ASTNode]) : Unit = {
    if(m.`object` == null) { // it is a main method
      Fail("VeyMont Fail: encountered method call %s in LTS generation",m.method)
    } else {
      if(m.method == chanWriteMethodName) {
        val argExp = m.args.head
        val sender = getNamesFromExpression(argExp).head
        val chan = m.`object`.asInstanceOf[NameExpression].name
        val receiver = chan.substring(sender.name.length,chan.length-chanName.length)
        val nextState = takeTransition(currentState, new LTSLabel(None, WriteAction(create.field_name(receiver),sender.name,argExp)), nextSeq)
        visitStatementSequence(nextState,nextSeq)
      } else if(m.method == barrierAwait) {
        Fail("VeyMont Fail: It is not allowed to use a method with name %s",barrierAwait)
      } else { //role method
        val nextState = takeTransition(currentState,new LTSLabel(None, SingleRoleAction(m)), nextSeq)
        visitStatementSequence(nextState,nextSeq)
      }
    }
  }

}
