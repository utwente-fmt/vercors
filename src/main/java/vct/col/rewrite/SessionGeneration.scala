package vct.col.rewrite

import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{Dereference, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.AbstractRewriter
import vct.col.util.SessionRolesAndMain
import vct.col.util.SessionRolesAndMain.{getChanName, getThreadClassName}

import java.util

class SessionGeneration(override val source: ProgramUnit, val session : SessionRolesAndMain) extends AbstractRewriter(null, true) {

  private var roleName : String = null
  var chans : Set[(String,String)] = Set()

  def getThreadsProgram() : ProgramUnit = {
    session.roleClasses.foreach(target().add(_))
    session.roleObjects.foreach(role => { //need to make create.new_class replacing this
      roleName = role.name
      val thread = new ASTClass(getThreadClassName(roleName), session.mainClass, this)
      thread.accept(this)
      target().add(thread.apply(this))
    })
    target()
  }

  override def visit(d : DeclarationStatement) = {
    if(session.roleObjects.contains(d) && d.name != roleName) {
      // remove d
    } else {
      super.visit(d);
    }
  }

  def getChanVar(role : NameExpression, isWrite : Boolean) =  create.name(NameExpressionKind.Unresolved, null, SessionRolesAndMain.getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  override def visit(a : AssignmentStatement) = {
    getDerefNameValid(false)(a.location) match {
      case Some(otherRole) => {
        if(expressionValid(true)(a.expression).nonEmpty) { //write a-exp to chan
          val chan = getChanVar(otherRole,true)
          chans += ((roleName,chan.name))
          result = create.invokation(chan, null, SessionRolesAndMain.chanWrite, a.expression)
        } else {
          // remove a
        }
      }
      case None =>
        expressionValid(false)(a.expression) match { //receive a-exp at chan
          case Some(n) => {
            val chan = getChanVar(n,false)
            chans += ((roleName,chan.name))
            result = create.invokation(chan,null, SessionRolesAndMain.chanRead)
          }
          case None => super.visit(a)
        }
    }
  }

  override def visit(e : OperatorExpression) ={
    e.operator match {
      case StandardOperator.Perm =>
        if(getDerefNameValid(false)(e.first).nonEmpty) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
      case _ =>
        if(e.args.exists(getDerefNameValid(false)(_).nonEmpty)) {
          result = create.constant(true)
        } else {
          super.visit(e)
        }
    }
  }

  def isSameOrOtherRole(isSame : Boolean)(n : NameExpression) = if(isSame) (n.name == roleName) else n.name != roleName
  def isDerefNameOtherRole(n : ASTNode) = getDerefNameValid(false)(n).nonEmpty

  def getDerefNameValid(isRole : Boolean)(n : ASTNode) : Option[NameExpression] = {
    n match {
      case d : Dereference => d.obj match {
        case n : NameExpression => Some(n).filter(isSameOrOtherRole(isRole))
        case _ => None
      }
      case _ => None
    }
  }

  def expressionValid(isRole : Boolean)(e : ASTNode) : Option[NameExpression] = {
    getDerefNameValid(isRole)(e) match {
      case Some(n) => Some(n)
      case None =>  e match {
        case o : OperatorExpression => o.args.map(expressionValid(isRole)).find(_.nonEmpty).flatten
        case _ => None
      }
    }
  }

}