package vct.col.rewrite

import hre.config.{Configuration, StringSetting}
import hre.lang.System.{Debug, Fail, Output}
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.print.PVLPrinter
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.syntax.PVLSyntax
import vct.col.ast.util.AbstractRewriter
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionStructureCheck.isExecutableMainMethod
import vct.col.util.SessionUtil.{barrierAwait, chanName, chanRead, chanWrite, getNameFromNode, getNamesFromExpression, getRoleName, getThreadClassName, isThreadClassName, mainMethodName, mapInsertSetValue, runMethodName, toLineString}

import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}
import java.io.{File, FileOutputStream, IOException, PrintWriter}

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
    case s : LTSState => this.toString == s.toString()
    case _ => false
  }
  override def hashCode(): Int = this.toString().hashCode()

  override def toString: String = nextStatements.map(PVLSyntax.get().print(_).toString).toString()

  def getCopy(copy_rw : AbstractRewriter) = new LTSState(copy_rw.rewrite(nextStatements.toArray).toList)
}
final class LTSLabel(val condition: Option[ASTNode], val action : Action) {
  override def toString: String = (condition match { case None => "true"; case Some(c) => c.toString}) + " @ " + action.toString
}
final class LTSTransition(val label : LTSLabel, val destState : LTSState) {
  override def toString: String = label.toString + " -> " + destState.toString
}

class SessionGenerateLTS(override val source : ProgramUnit, isGlobal : Boolean) extends AbstractRewriter(null, true){

  private var initialState : LTSState = null
  private var transitions : Map[LTSState,Set[LTSTransition]] = Map()
  private var roleNames : Iterable[String] = null
  private var mainMethods : Iterable[Method] = null
  private var roleName : String = null

  private val sessionFileName = Configuration.session_file.get()
  private val session_global_lts = sessionFileName.slice(0,sessionFileName.length-4) + "GlobalLTS.aut"
  private def session_local_lts : String = sessionFileName.slice(0,sessionFileName.length-4) + roleName + "LocalLTS.aut"

  def generateLTSAndPrint() : Unit = {
    if(isGlobal) {
      mainMethods = SessionStructureCheck.getMainClass(source).methods().filter(isExecutableMainMethod)
      roleNames = SessionStructureCheck.getRoleNames(source)
      generateLTS(SessionStructureCheck.getMainClass(source))
      print()
    } else {
      val roleClasses = source.get().filter(n => isThreadClassName(n.name)).map(_.asInstanceOf[ASTClass])
      roleNames = roleClasses.map(c => getRoleName(c.name))
      roleClasses.foreach{ thread =>
        initialState = null
        transitions = Map()
        mainMethods = thread.methods().filter(isExecutableMainMethod)
        roleName = thread.fields().head.name
        generateLTS(thread)
        print()
      }
    }
  }

  private def print(out : PrintWriter) : Unit = {
    val destStates : Set[LTSState] = transitions.values.flatMap(_.map(_.destState)).toSet
    val states : Set[LTSState] = transitions.keys.toSet ++ destStates
    val stateMap : Map[LTSState,Int] = states.zipWithIndex.toMap
    val nrTrans = transitions.values.map(_.size).sum
    out.println("des (" + stateMap(initialState) + "," + nrTrans + "," + states.size + ")")
    transitions foreach { case (src, trset) =>
      trset.foreach(tr => {
        out.println("(" + stateMap(src) + ",\"" + tr.label + "\"," + stateMap(tr.destState) + ")")
      })
    }
    //out.println("\nStates:")
    //stateMap.foreach{ case (s,i) => out.println(i + ": " + s)}
  }

  private def print() : Unit = {
    try {
      val f = if(isGlobal) new File(session_global_lts) else new File(session_local_lts);
      val b = f.createNewFile();
      if (!b) {
        Debug("File %s already exists and is now overwritten", f.toString);
      }
      val out = new PrintWriter(new FileOutputStream(f));
      print(out)
      out.close();
    } catch {
      case e: IOException => Debug(e.getMessage);
    }
  }

  private def generateLTS(classDef : ASTClass) : Unit = {
    //val constructorStats = mainClass.methods().find(_.kind == Method.Kind.Constructor).get.getBody.asInstanceOf[BlockStatement].getStatements.toList
    val runMethodStats = classDef.methods().find(_.name == runMethodName).get.getBody.asInstanceOf[BlockStatement].getStatements.toList
    initialState = new LTSState(runMethodStats).getCopy(copy_rw)
    visitStatementSequence(initialState,initialState.nextStatements)
  }

  override def visit(m : Method) : Unit =
    Fail("Session Fail: method visit(Method) should not be reached")

  override def visit(b : BlockStatement) : Unit =
    Fail("Session Fail: method visit(BlockStatement) should not be reached")

  def visitStatementSequence(currentState : LTSState, seq : List[ASTNode]) : Unit = {
    if(seq.nonEmpty && !transitions.contains(currentState)) {
      val s1 = seq.head
      val nextSeq = seq.tail
      visitStatementSequenceAbstract(currentState,s1,nextSeq,if (nextSeq.isEmpty) None else Some((nextSeq.head,nextSeq.tail)))
    /*  visitNode(s1, currentState, nextSeq)
      if(nextSeq.nonEmpty) {
        val s2 = nextSeq.head
        if (weakSequenceAllowed(s1, s2)) { //if weak sequence allowed: also do other order s2;s1
          val nextSeq2 = s1 +: nextSeq.tail
          visitNode(s2, currentState, nextSeq2)
        }
      } */
    }
  }

  def visitStatementSequenceAbstract(currentState : LTSState, s1 : ASTNode, s1NextSeq : List[ASTNode], s2Opt : Option[(ASTNode, List[ASTNode])]) = {
    visitNode(s1, currentState, s1NextSeq)
    s2Opt match {
      case Some((s2,s2NextSeq)) =>
        if (weakSequenceAllowed(s1, s2)) { //if weak sequence allowed: also do other order s2;s1
          visitNode(s2, currentState, s2NextSeq)
        }
      case None => //do nothing
    }
  }

  def visitNode(n : ASTNode, currentState : LTSState, seqAfterFirstStatement : List[ASTNode]) : Unit = n match {
    case a : AssignmentStatement => visit(a,currentState,seqAfterFirstStatement)
    case i : IfStatement => visit(i,currentState,seqAfterFirstStatement)
    case l : LoopStatement => visit(l,currentState,seqAfterFirstStatement)
    case p : ParallelRegion => visit(p,currentState,seqAfterFirstStatement)
    case m : MethodInvokation => visit(m,currentState,seqAfterFirstStatement)
    case s : ASTSpecial => visit(s,currentState,seqAfterFirstStatement)
    case _ => Fail("Session Fail: cannot visit this type of statement!")
  }

  def takeTransition(currentState : LTSState, label : LTSLabel, nextStateSeq : List[ASTNode]) : LTSState = {
    val nextState = new LTSState(nextStateSeq)
    Output(label.action.toString.replace('%','^') + " " + (if(label.action == BarrierWait) label.condition.get else ""))
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
            case _ => Fail("Session Fail: cannot determine subject of  assignment " + a.toString); Set.empty
          }
          case m : MethodInvokation => m.`object` match {
            case n : NameExpression => Set(n.name)
            case _ => Fail("Session Fail: cannot determine subject of  assignment " + a.toString); Set.empty
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
        if(m.`object` == null) //it is a main method
          getNamesFromExpression(m).map(_.name) ++ (if(seen.contains(m.method)) Set.empty else getSubjects(seen + m.method,getMethodFromCall(m).getBody.asInstanceOf[BlockStatement].head))
        else getNamesFromExpression(m).map(_.name)
      }
      case _ : ASTSpecial => roleNames.toSet
    }
  }

  def canSwap(n : ASTNode) : Boolean = n match {
    case _ : IfStatement => false
    case _ : LoopStatement => false
    case _ : ASTSpecial => false
    case _ => true
  }

  def visit(s : ASTSpecial, currentState : LTSState, seq : List[ASTNode]) : Unit =
    if (s.kind == ASTSpecial.Kind.TauAction) {
      val nextState = takeTransition(currentState, new LTSLabel(None,Tau), seq)
      visitStatementSequence(nextState, seq)
    } else Fail("Session Fail: cannot visit this type of statement!")


  def mapInsertTransition(k : LTSState, v : LTSTransition) : Map[LTSState,Set[LTSTransition]] =
    mapInsertSetValue[LTSState,LTSTransition](k,v,transitions)

  def visit(a : AssignmentStatement, currentState : LTSState, seq : List[ASTNode]) = {
    val nextState = takeTransition(currentState, new LTSLabel(None,if(isGlobal) getGlobalAction(a) else getLocalAction(a,roleName)), seq)
    visitStatementSequence(nextState,seq)
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
          SingleRoleAction(a)
        else if(expRole.size == 1 && expRole.head.name != locRole.name)
          CommunicationAction(locRole,a.location.asInstanceOf[Dereference].field,expRole.head,a.expression)
        else ErrorAction
      }
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
    case m : MethodInvokation => m.method == chanRead
    case _ => false
  }

  def visit(i: IfStatement, currentState : LTSState, seq : List[ASTNode]) : Unit =
    takeTwoBranches(i.getGuard(0),ASTNodeToList(i.getStatement(0)),if(i.getCount > 1) ASTNodeToList(i.getStatement(1)) else List.empty,currentState,seq)

  override def visit(i: IfStatement) =
    Fail("Session Fail: method visit(IfStatement) should not be reached")

  def isRecursion(s : ASTNode, recursiveNodes : List[ASTNode]) : Boolean = recursiveNodes.exists(n => new LTSState(List(n)).toString == new LTSState(List(s)).toString)

  def visit(l : LoopStatement, currentState : LTSState, seq : List[ASTNode]) : Unit =
      takeTwoBranches(l.getEntryGuard,ASTNodeToList(l.getBody) :+ l,List.empty,currentState,seq)

  override def visit(l : LoopStatement) =
    Fail("Session Fail: method visit(LoopStatement) should not be reached")

  private def ASTNodeToList(n : ASTNode) : List[ASTNode] = n match {
    case b : BlockStatement => b.getStatements.toList
    case a : ASTNode => List(a)
  }

  //visit(new IfStatement(l.getEntryGuard,create.block(l.getBody,l),new BlockStatement()))

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

  override def visit(pr : ParallelRegion) : Unit =
    Fail("Session Fail: method visit(ParallelRegion) should not be reached")

  def visit(pr : ParallelRegion, currentState : LTSState, nextSeq : List[ASTNode]) : Unit = {
    pr.blocks.indices.foreach(i => {
      val split = pr.blocks.splitAt(i)
      val b = split._2.head
      val others = split._1 ++ split._2.tail
      val s1 = b.block.getStatement(0)
      val nextSeqb = b.block.getStatements.tail
      val s1NextSeq = getNewPrBeforeNextSeq(b, nextSeqb,others,pr, nextSeq)
      val s2Opt = if (nextSeqb.isEmpty) None else {
        Some((nextSeqb.head,getNewPrBeforeNextSeq(b,s1 +: nextSeqb.tail,others,pr,nextSeq)))
      }
      visitStatementSequenceAbstract(currentState,s1,s1NextSeq,s2Opt)
      })
  /*  nextSequences.foreach(seq => {
      visitStatementSequence(currentState,seq,true)
    }) */
  }

  private def getNewPrBeforeNextSeq(b : ParallelBlock, seq : Array[ASTNode], others : List[ParallelBlock], pr : ParallelRegion, nextSeq : List[ASTNode]) : List[ASTNode] = {
    val afterActions2 = getCopyBlockWithStats(b,seq)
    val newPr = create.region(pr.contract,(if (afterActions2.block.isEmpty) others else afterActions2 +: others):_*)
    if(newPr.blocks.forall(_.block.size == 0)) nextSeq else newPr +: nextSeq
  }

  private def getCopyBlockWithStats(pb : ParallelBlock, statements : Array[ASTNode]) : ParallelBlock =
    create.parallel_block(pb.label,pb.contract,pb.itersJava,create.block(copy_rw.rewrite(statements):_*), pb.deps)

  override def visit(m : MethodInvokation) : Unit =
    Fail("Session Fail: method visit(MethodInvokation) should not be reached")

  def visit(m : MethodInvokation, currentState : LTSState, nextSeq : List[ASTNode]) = {
    if(m.`object` == null) { // it is a main method
      // if(!isRecursion(m)) {
        visitStatementSequence(currentState,ASTNodeToList(getMethodFromCall(m).getBody) ++ nextSeq, false)
    } else {
      if(m.method == chanWrite) {
        val argExp = m.args.head
        val sender = getNamesFromExpression(argExp).head
        val chan = m.`object`.asInstanceOf[NameExpression].name
        val receiver = chan.substring(sender.name.length,chan.length-chanName.length)
        val nextState = takeTransition(currentState, new LTSLabel(None, WriteAction(create.field_name(receiver),sender.name,argExp)), nextSeq)
        visitStatementSequence(nextState,nextSeq)
      } else if(m.method == barrierAwait) {
        Fail("Session Fail: Barrier!!!")
      } else { //role method
        val nextState = takeTransition(currentState,new LTSLabel(None, SingleRoleAction(m)), nextSeq)
        visitStatementSequence(nextState,nextSeq)
      }
    }
  }

  private def getMethodFromCall(m : MethodInvokation) : Method = {
    val method = mainMethods.filter(md => md.name == m.method && m.getArity == md.getArity)
    if(method.size != 1)
      Fail("Session: Fail could not find method definition for method " + m.method)
    method.head
  }


}
