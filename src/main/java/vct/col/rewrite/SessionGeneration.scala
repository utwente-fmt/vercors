package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, IfStatementCase, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, Contract, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{barrierAwait, barrierFieldName, chanRead, chanWrite, getArgName, getBarrierClass, getChanName, getNameFromNode, getNamesFromExpression, getThreadClassName}

class SessionGeneration(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private val roleObjects : Array[AssignmentStatement] = SessionStructureCheck.getRoleObjects(source)
  private val mainClass = SessionStructureCheck.getMainClass(source)
  private var roleName : String = null

  def addThreadClasses() = {
    SessionStructureCheck.getNonMainClasses(source).foreach(target().add(_))
    roleObjects.foreach(role => {
      target().add(createThreadClass(role))
    })
  }

  private def createThreadClass(role : AssignmentStatement) = {
    create.enter()
    roleName = role.location.asInstanceOf[NameExpression].name
    create.setOrigin(new MessageOrigin("Generated thread class " + roleName))
    val threadName = getThreadClassName(roleName)
    val thread = create.new_class(threadName,null,null)
    mainClass.methods().forEach(m => thread.add_dynamic(rewrite(m)))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add_dynamic(rewrite(f)))
    thread.add_dynamic(create.field_decl(barrierFieldName,getBarrierClass))
    create.leave()
    thread
  }

  override def visit(m : Method) = {
    if(m.kind == Method.Kind.Constructor) {
      val newContract = new ContractBuilder()
      val barrierArgField = create.field_name(getArgName(barrierFieldName))
      newContract.requires(create.expression(StandardOperator.NEQ,create.field_name(getArgName(barrierFieldName)),create.reserved_name(ASTReserved.Null)))
      getBarrierAnnotations().foreach(newContract.ensures(_))
      rewrite(m.getContract,newContract)
      m.getBody match {
        case b : BlockStatement => {
          val newArgs = rewrite(m.getArgs) :+ create.field_decl(getArgName(barrierFieldName),getBarrierClass())
          val barAssign = create.assignment(create.field_name(barrierFieldName),create.field_name(getArgName(barrierFieldName)))
          val newBody = create.block(rewrite(b.getStatements) :+ barAssign:_*)
          result = create.method_kind(m.kind,m.getReturnType,newContract.getContract,getThreadClassName(roleName),newArgs,newBody)
        }
        case _ => Fail("Session Fail: expected BlockStatement in Method %s",m.name)
      }

    } else {
      val newContract = new ContractBuilder()
      getBarrierAnnotations().foreach(newContract.context(_))
      rewrite(m.getContract,newContract)
      result = create.method_kind(m.kind,m.getReturnType,newContract.getContract,m.name,m.getArgs,rewrite(m.getBody))
    }
  }

  override def visit(a : AssignmentStatement) = {
    getValidNameFromNode(false, a.location) match {
      case Some(otherRole) => {
        if(getValidNameFromExpression(true, a.expression).nonEmpty) { //write a-exp to chan
          val chan = getChanVar(otherRole,true)
          result = create.invokation(chan, null, chanWrite, a.expression)
        } else {
          // remove a
        }
      }
      case None =>
        getValidNameFromExpression(false, a.expression) match { //receive a-exp at chan
          case Some(n) => {
            val chan = getChanVar(n,false)
            result = create.assignment(a.location,create.invokation(chan,null, chanRead))
          }
          case None => super.visit(a)
        }
    }
  }

  override def visit(e : OperatorExpression) ={
    e.operator match {
      case StandardOperator.Perm =>
        if(getValidNameFromNode(false, e.first).nonEmpty) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
      case _ =>
        if(e.args.exists(getValidNameFromNode(false, _).nonEmpty)) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
    }
  }

  override def visit(s : IfStatement) = {
    val stats : Seq[BlockStatement] = (0 until s.getCount).map(s.getStatement).filter {
      case b: BlockStatement => true
      case _ => Fail("Session Fail: expected BlockStatement in IfStatementCase"); false
    }.asInstanceOf[Seq[BlockStatement]]
      .map(b => create.block(prependBarrier(b): _*))
    result = create.ifthenelse(rewrite(s.getGuard(0)),stats:_*)
  }

  override def visit(l : LoopStatement) = {
    l.getBody match {
      case b : BlockStatement => {
        val newContract = new ContractBuilder()
        getBarrierAnnotations().foreach(newContract.appendInvariant(_))
        rewrite(l.getContract,newContract)
        result = create.while_loop(rewrite(l.getEntryGuard),create.block(prependBarrier(b):_*),newContract.getContract)
      }
      case _ => Fail("Session Fail: expected BlockStatement in LoopStatement")
    }
  }

  override def visit(b : BlockStatement) = {
    val nrLoops = b.getStatements.count(_.isInstanceOf[LoopStatement])
    if(nrLoops > 0) {
      val newStats = new Array[ASTNode](b.getLength+nrLoops)
      var i = 0
      for(stat <- b.getStatements) {
        newStats(i) = rewrite(stat)
        i = i+1
        if(stat.isInstanceOf[LoopStatement]) {
          newStats(i) = getBarrierInvokation()
          i = i+1
        }
      }
      result = create.block(newStats:_*)
    } else {
      super.visit(b)
    }
  }

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.name(NameExpressionKind.Unresolved, null, getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def getValidNameFromNode(isRole : Boolean, n : ASTNode) : Option[NameExpression] =
    getNameFromNode(n).filter(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getValidNameFromExpression(isRole : Boolean, e : ASTNode) : Option[NameExpression] =
    getNamesFromExpression(e).find(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getBarrierInvokation() = create.invokation(create.field_name(barrierFieldName), null, barrierAwait)

  private def prependBarrier(b : BlockStatement) : Array[ASTNode] = getBarrierInvokation() +: rewrite(b.getStatements)

  private def getBarrierAnnotations() : List[OperatorExpression] = {
    List(create.expression(StandardOperator.Perm,create.field_name(barrierFieldName),create.reserved_name(ASTReserved.ReadPerm)),
    create.expression(StandardOperator.NEQ,create.field_name(barrierFieldName),create.reserved_name(ASTReserved.Null)))
  }

}