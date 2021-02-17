package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
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
    mainClass.methods().forEach(m => thread.add(rewrite(m)))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add(rewrite(f)))
    create.leave()
    thread
  }

  override def visit(m : Method) = {
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind,m.getReturnType,rewrite(m.getContract),getThreadClassName(roleName),m.getArgs,rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Pure) {
      result = copy_rw.rewrite(m)
    } else {
      super.visit(m)
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
    if(e.operator == StandardOperator.Perm) {
        if(getValidNameFromNode(false, e.first).nonEmpty) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
    } else {
        if(e.args.exists(getValidNameFromNode(false, _).nonEmpty)) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
    }
  }

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.name(NameExpressionKind.Unresolved, null, getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def getValidNameFromNode(isRole : Boolean, n : ASTNode) : Option[NameExpression] =
    getNameFromNode(n).filter(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getValidNameFromExpression(isRole : Boolean, e : ASTNode) : Option[NameExpression] =
    getNamesFromExpression(e).find(n => if(isRole) (n.name == roleName) else n.name != roleName)

}