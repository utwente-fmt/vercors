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
import vct.col.util.SessionUtil.{barrierAwait, barrierFieldName, chanRead, chanWrite, getArgName, getBarrierClass, getChanClass, getChanName, getNameFromNode, getNamesFromExpression, getThreadClassName, mainClassName}

import scala.collection.JavaConversions._

class SessionGeneration(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private val roleObjects : Array[AssignmentStatement] = SessionStructureCheck.getRoleObjects(source)
  private val mainClass = SessionStructureCheck.getMainClass(source)
  private var roleName : String = "Error No Role Name!"

  private var chans : Set[String] = Set()

  def addThreadClasses() : ProgramUnit = {
    source.get().filter(_.name != mainClassName).foreach(target().add(_))
    roleObjects.foreach(role => {
      target().add(createThreadClass(role))
    })
    target()
  }

  private def createThreadClass(role : AssignmentStatement) = {
    create.enter()
    roleName = role.location.asInstanceOf[NameExpression].name
    chans = Set()
    create.setOrigin(new MessageOrigin("Generated thread class " + roleName))
    val threadName = getThreadClassName(roleName)
    val thread = create.new_class(threadName,null,null)
    val rewMethods = mainClass.methods().map(rewrite(_))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add(rewrite(f)))
    chans.foreach(chan => thread.add_dynamic(create.field_decl(chan,getChanClass())))
    thread.add_dynamic(create.field_decl(barrierFieldName,getBarrierClass()))
    rewMethods.foreach(thread.add)
    create.leave()
    thread
  }

  override def visit(m : Method) : Unit = { //assume ony pre and postconditions
    val c = m.getContract()
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectAnnotation(c.pre_condition)))
    cb.ensures(rewrite(selectAnnotation(c.post_condition)))
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,getThreadClassName(roleName),m.getArgs,rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Pure) {
      result = copy_rw.rewrite(m)
    } else {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,m.name,m.getArgs,rewrite(m.getBody))
    }
  }

  override def visit(l : LoopStatement) : Unit = { //assume while loop
    val c = l.getContract
    val cb = new ContractBuilder()
    cb.appendInvariant(rewrite(selectAnnotation(c.invariant)))
    result = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract)
  }

  override def visit(pb : ParallelBlock) : Unit = {
    val c = pb.contract
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectAnnotation(c.pre_condition)))
    cb.ensures(rewrite(selectAnnotation(c.post_condition)))
    result = create.parallel_block(pb.label,cb.getContract,pb.itersJava,rewrite(pb.block),pb.deps)
  }

  override def visit(a : AssignmentStatement) : Unit = {
    getValidNameFromNode(false, a.location) match {
      case Some(otherRole) =>
        if(getValidNameFromExpression(true, a.expression).nonEmpty) { //write a-exp to chan
          val chan = getChanVar(otherRole,true)
          chans += chan.name
          result = create.invokation(chan, null, chanWrite, a.expression)
        } else {
          // remove a
        }
      case None =>
        getValidNameFromExpression(false, a.expression) match { //receive a-exp at chan
          case Some(n) => {
            val chan = getChanVar(n,false)
            chans += chan.name
            result = create.assignment(a.location,create.invokation(chan,null, chanRead))
          }
          case None => super.visit(a)
        }
    }
  }

  override def visit(e : OperatorExpression) : Unit ={
    if(e.operator == StandardOperator.Star || e.operator == StandardOperator.And) {
      result = create.expression(e.operator,rewrite(e.first),rewrite(e.second))
    } else getValidNameFromExpression(true,e) match {
      case Some(_) => result = copy_rw.rewrite(e)
      case None => result = create.constant(true)
    }
  }

  private def selectAnnotation(n :ASTNode) : ASTNode =
    n match {
      case e : OperatorExpression => e.operator match {
        case StandardOperator.Perm => n
        case StandardOperator.NEQ => if (isNullNode(e.first) || isNullNode(e.second)) n else create.constant(true)
        case StandardOperator.Star => create.expression(e.operator,selectAnnotation(e.first), selectAnnotation(e.second))
        case _ => create.constant(true)
      }
      case _ => create.constant(true)
    }

  private def isNullNode(n : ASTNode) : Boolean =
    n match {
      case name : NameExpression => name.reserved == ASTReserved.Null
      case _ => false
    }

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.field_name(getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def getValidNameFromNode(isRole : Boolean, n : ASTNode) : Option[NameExpression] =
    getNameFromNode(n).filter(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getValidNameFromExpression(isRole : Boolean, e : ASTNode) : Option[NameExpression] =
    getNamesFromExpression(e).find(n => if(isRole) (n.name == roleName) else n.name != roleName)

}