package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, IfStatementCase, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, Contract, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{barrierAwait, barrierFieldName, chanRead, chanWrite, getArgName, getBarrierClass, getChanName, getNameFromNode, getNamesFromExpression, getThreadClassName}

import scala.collection.JavaConversions._

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
    mainClass.methods().forEach(m => thread.add(rewrite(m)))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add(rewrite(f)))
    create.leave()
    thread
  }

  override def visit(m : Method) = { //assume ony pre and postconditions
    val c = m.getContract()
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectPerms(c.pre_condition)))
    cb.ensures(rewrite(selectPerms(c.post_condition)))
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,getThreadClassName(roleName),m.getArgs,rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Pure) {
      result = copy_rw.rewrite(m)
    } else {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,m.name,m.getArgs,rewrite(m.getBody))
    }
  }

  override def visit(l : LoopStatement) = { //assume while loop
    val c = l.getContract()
    val cb = new ContractBuilder()
    cb.appendInvariant(rewrite(selectPerms(c.invariant)))
    result = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract)
  }

  override def visit(pb : ParallelBlock) = {
    val c = pb.contract
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectPerms(c.pre_condition)))
    cb.ensures(rewrite(selectPerms(c.post_condition)))
    result = create.parallel_block(pb.label,cb.getContract,pb.itersJava,rewrite(pb.block),pb.deps)
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
    if(e.operator == StandardOperator.Star) {
      super.visit(e);
    } else if(e.operator == StandardOperator.Not) {
      result = create.expression(e.operator,rewriteConditionArg(e.first))
    } else if(e.operator == StandardOperator.And) {
      result = create.expression(e.operator,rewriteConditionArg(e.first),rewriteConditionArg(e.second))
    } else {
      result = copy_rw.rewrite(e)
    }
  }

  private def rewriteConditionArg(n :ASTNode) : ASTNode=
    if (getValidNameFromExpression(true,n).nonEmpty)
      rewrite(n)
    else create.constant(true)

  private def selectPerms(n : ASTNode) : ASTNode = {
    val conj = ASTUtils.conjuncts(n, StandardOperator.Star).filter {
      case e: OperatorExpression => e.operator == StandardOperator.Perm && getValidNameFromNode(true,e.first).nonEmpty
      case _ => false
    }
    if (conj.isEmpty) create.constant(true)
    else {
      conj.reduce((l, r) => create.expression(StandardOperator.Star, l, r))
    }
  }

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.name(NameExpressionKind.Unresolved, null, getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def getValidNameFromNode(isRole : Boolean, n : ASTNode) : Option[NameExpression] =
    getNameFromNode(n).filter(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getValidNameFromExpression(isRole : Boolean, e : ASTNode) : Option[NameExpression] =
    getNamesFromExpression(e).find(n => if(isRole) (n.name == roleName) else n.name != roleName)

}