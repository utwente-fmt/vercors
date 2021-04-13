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

final class LTSSequence(val currentState : LTSState, val sequence: List[ASTNode], var recursiveNodes : List[ASTNode]) {
  override def equals(that: Any): Boolean = that match {
    case s : LTSSequence => currentState == s.currentState && new LTSState(sequence) == new LTSState(s.sequence)
    case _ => false
  }
  override def hashCode(): Int = this.toString.hashCode
  override def toString: String = currentState.toString + new LTSState(sequence).toString
}

class SessionGlobalLTS(override val source : ProgramUnit, isGlobal : Boolean) extends AbstractRewriter(null, true){

  private var initialState : LTSState = null
  private var transitions : Map[LTSState,Set[LTSTransition]] = Map()
  private var roleNames : Iterable[String] = null
  private var mainMethods : Iterable[Method] = null
  private var roleName : String = null
  private var todo : Set[LTSSequence] = null
  private var done : Set[LTSSequence] = null

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
        roleName = getRoleName(thread.name)
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
    todo = Set(new LTSSequence(initialState,initialState.nextStatements,List.empty))
    done = Set.empty
    visitAllSequences()
  }

  private def visitAllSequences() : Unit = {
    while(todo.nonEmpty) {
      val exp = todo.head
      done = done + exp
      todo = todo - exp
      visitStatementSequence(exp)
    }
  }

  private def addTodo(exp : LTSSequence) : Unit =
    if(exp.sequence.nonEmpty && !done.contains(exp)) {
      todo = todo + exp
      Output("todo: " + todo.size)
    }

  override def visit(m : Method) : Unit =
    Fail("Session Fail: method visit(Method) should not be reached")

  override def visit(b : BlockStatement) : Unit =
    Fail("Session Fail: method visit(BlockStatement) should not be reached")

  def visitStatementSequence(exp : LTSSequence) : Unit = {
    if(!isRecursion(exp.sequence.head,exp.recursiveNodes)) { //|| !transitions.contains(exp.currentState)
      val s1 = exp.sequence.head
      val nextSeq = exp.sequence.tail
      visitNode(s1, new LTSSequence(exp.currentState,nextSeq,exp.recursiveNodes))
      if(nextSeq.nonEmpty) {
        val s2 = nextSeq.head
        if (weakSequenceAllowed(s1, s2)) { //if weak sequence allowed: also do other order s2;s1
          addTodo(new LTSSequence(exp.currentState,s2 +: s1 +: nextSeq.tail,exp.recursiveNodes))
        }
      }
    }
  }

  def visitNode(n : ASTNode, exp : LTSSequence) : Unit = n match {
    case a : AssignmentStatement => visit(a,exp)
    case i : IfStatement => visit(i,exp)
    case l : LoopStatement => visit(l,exp)
    case p : ParallelRegion => visit(p,exp)
    case m : MethodInvokation => visit(m,exp)
    case s : ASTSpecial => visit(s,exp)
    case _ => Fail("Session Fail: cannot visit this type of statement!")
  }

  def takeTransition(label : LTSLabel, exp : LTSSequence) = {
    val nextState = new LTSState(exp.sequence)
    Output(label.action.toString.replace('%','^'))
    transitions = mapInsertTransition(exp.currentState, new LTSTransition(label,nextState))
    addTodo(new LTSSequence(nextState,exp.sequence,exp.recursiveNodes))
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

  def visit(s : ASTSpecial, exp : LTSSequence) : Unit =
    if (s.kind == ASTSpecial.Kind.TauAction) {
      takeTransition(new LTSLabel(None,Tau), exp)
    } else Fail("Session Fail: cannot visit this type of statement!")


  def mapInsertTransition(k : LTSState, v : LTSTransition) : Map[LTSState,Set[LTSTransition]] =
    mapInsertSetValue[LTSState,LTSTransition](k,v,transitions)

  def visit(a : AssignmentStatement, exp : LTSSequence) : Unit =
    takeTransition(new LTSLabel(None,if(isGlobal) getGlobalAction(a) else getLocalAction(a,roleName)), exp)

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

  def visit(i: IfStatement, exp : LTSSequence) : Unit =
    takeTwoBranches(i.getGuard(0),ASTNodeToList(i.getStatement(0)),if(i.getCount > 1) ASTNodeToList(i.getStatement(1)) else List.empty,exp)

  override def visit(i: IfStatement) =
    Fail("Session Fail: method visit(IfStatement) should not be reached")

  def isRecursion(s : ASTNode, recursiveNodes : List[ASTNode]) : Boolean = recursiveNodes.exists(n => new LTSState(List(n)).toString == new LTSState(List(s)).toString)

  def visit(l : LoopStatement, exp : LTSSequence) : Unit = {
      exp.recursiveNodes = l +: exp.recursiveNodes
      takeTwoBranches(l.getEntryGuard,ASTNodeToList(l.getBody) :+ l,List.empty,exp)
  }

  override def visit(l : LoopStatement) =
    Fail("Session Fail: method visit(LoopStatement) should not be reached")

  private def ASTNodeToList(n : ASTNode) : List[ASTNode] = n match {
    case b : BlockStatement => b.getStatements.toList
    case a : ASTNode => List(a)
  }

  //visit(new IfStatement(l.getEntryGuard,create.block(l.getBody,l),new BlockStatement()))

  private def takeTwoBranches(cond : ASTNode, leftBranch : List[ASTNode], rightBranch : List[ASTNode], exp : LTSSequence) : Unit = {
    val condTrue = new LTSLabel(Some(cond),BarrierWait)
    takeIfBranch(condTrue,leftBranch,exp)
    val condFalse = new LTSLabel(Some(OperatorExpression(StandardOperator.Not,List(cond))),BarrierWait)
    takeIfBranch(condFalse,rightBranch,exp)
  }

  private def takeIfBranch(condLabel : LTSLabel, statements : List[ASTNode], exp : LTSSequence) : Unit =
    takeTransition(condLabel,new LTSSequence(exp.currentState,statements ++ exp.sequence,exp.recursiveNodes))

  override def visit(pr : ParallelRegion) : Unit =
    Fail("Session Fail: method visit(ParallelRegion) should not be reached")

  private def getWeakSequences(seq : List[ASTNode]) : List[List[ASTNode]] = {
    if(seq.size <= 1) {
      List(seq)
    } else {
      val s1 = seq.head
      val s2 = seq.tail.head
      val strongSeqList = for (l <- getWeakSequences(seq.tail)) yield s1 +: l
      if(weakSequenceAllowed(s1,s2)) {
        val weakSeqList = for (l <- getWeakSequences(s1 +: seq.tail.tail)) yield s2 +: l
        strongSeqList ++ weakSeqList
      } else strongSeqList
    }
  }
                                    //blockWeakseq                  ListBlockSeq
  private def getCombinations(blocks : List[List[List[ASTNode]]]) : List[List[List[ASTNode]]] = {
    if (blocks.forall(_.isEmpty)) {
      List.empty
    } else if (blocks.forall(_.size <= 1)) {
      List(blocks.map(b => if (b.isEmpty) List.empty else b.head))
    } else {
      blocks.indices.map(i => {
        val split = blocks.splitAt(i)
        val blist : List[List[ASTNode]] = split._2.head
        val otherBlocks = split._1 ++ split._2.tail
        for (seq : List[ASTNode] <- blist; comb : List[List[ASTNode]] <- getCombinations(otherBlocks)) yield seq +: comb
      }).reduce((l1,l2) => l1 ++ l2)
    }
  }

  private def getInterLeavings(seqs : List[List[ASTNode]]) : List[List[ASTNode]] = {
    val neseqs = seqs.filter(_.nonEmpty)
    if(neseqs.size > 1) {
      neseqs.indices.map(i => {
        val split = neseqs.splitAt(i)
        val seq = split._2.head
        val others = split._1 ++ split._2.tail
        for (l <- getInterLeavings(seq.tail +: others)) yield seq.head +: l
      }).reduce((l1,l2) => l1 ++ l2)
    } else neseqs
  }

  def visit(pr : ParallelRegion, exp : LTSSequence) : Unit = {
    val nextSequences = pr.blocks.indices.map(i => {
      val split = pr.blocks.splitAt(i)
      (split._2.head, split._1 ++ split._2.tail)
    }).map{ case (b,others) => {
      val afterAction = getTailParBlock(b)
      val newPr = create.region(pr.contract,(if (afterAction.block.isEmpty) others else afterAction +: others):_*)
      if(newPr.blocks.forall(_.block.size == 0))
        b.block.getStatement(0) +: exp.sequence
      else
        b.block.getStatement(0) +: newPr +: exp.sequence
    }}
    nextSequences.foreach(seq => {
      addTodo(new LTSSequence(exp.currentState,seq,exp.recursiveNodes))
    })
  }

  private def getCopyBlockWithStats(pb : ParallelBlock, statements : Array[ASTNode]) : ParallelBlock =
    create.parallel_block(pb.label,pb.contract,pb.itersJava,create.block(copy_rw.rewrite(statements):_*), pb.deps)

  private def getTailParBlock(pb : ParallelBlock) : ParallelBlock = getCopyBlockWithStats(pb,pb.block.getStatements.tail)

  override def visit(m : MethodInvokation) : Unit =
    Fail("Session Fail: method visit(MethodInvokation) should not be reached")

  def visit(m : MethodInvokation, exp : LTSSequence) = {
    if(m.`object` == null) { // it is a main method
        addTodo(new LTSSequence(exp.currentState,ASTNodeToList(getMethodFromCall(m).getBody) ++ exp.sequence,m +: exp.recursiveNodes))
    } else {
      if(m.method == chanWrite) {
        val argExp = m.args.head
        val sender = getNamesFromExpression(argExp).head
        val chan = m.`object`.asInstanceOf[NameExpression].name
        val receiver = chan.substring(sender.name.length,chan.length-chanName.length)
        takeTransition(new LTSLabel(None, WriteAction(create.field_name(receiver),sender.name,argExp)), exp)
      } else if(m.method == barrierAwait) {
        Fail("Session Fail: Barrier!!!")
      } else { //role method
        takeTransition(new LTSLabel(None, SingleRoleAction(m)), exp)
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
