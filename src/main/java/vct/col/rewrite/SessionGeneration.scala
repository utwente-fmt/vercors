package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTClass, Contract, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{chanRead, chanWrite, getChanName, getNameFromNode, getNamesFromExpression, getThreadClassName}

import java.util
import java.util.Iterator
import scala.collection.JavaConversions.`deprecated asScalaIterator`

class SessionGeneration(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private val roleObjects : Array[AssignmentStatement] = SessionStructureCheck.getRoleObjects(source)
  private val mainClass = SessionStructureCheck.getMainClass(source)
  private val roleClasses : Iterable[ASTClass] = SessionStructureCheck.getRoleClasses(source)
  private var roleName : String = null
  private var rolePerms : Contract = null
  private var chans : Set[(String,String)] = Set()

  def addThreadClasses() = {
    SessionStructureCheck.getNonMainClasses(source).foreach(target().add(_))
    roleObjects.foreach(role => {
      target().add(rewrite(createThreadClass(role)))
    })
  }

  private def createThreadClass(role : AssignmentStatement) = {
    create.enter()
    roleName = role.location.asInstanceOf[NameExpression].name
    create.setOrigin(new MessageOrigin("Generated thread class " + roleName))
    rolePerms = getRolePermissions(role)
    val threadName = getThreadClassName(roleName)
    val thread = create.new_class(threadName,null,null)
    mainClass.methods().forEach(m => thread.add_dynamic(rewrite(m)))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add_dynamic(rewrite(f)))
    create.leave()
    thread
  }

  private def getRolePermissions(role : AssignmentStatement) = {
    val contr = new ContractBuilder()
    val readPerm =
      create.expression(StandardOperator.Perm,
        create.field_name(roleName),
        create.reserved_name(ASTReserved.ReadPerm))
    contr.ensures(readPerm)
    val roleType = role.expression.asInstanceOf[MethodInvokation].dispatch.getName
    roleClasses.find(_.name == roleType) match {
      case None => Fail("Session Fail : cannot find class %s", roleType)
      case Some(c) => {
        c.methods().iterator().find(_.kind == Method.Kind.Constructor) match {
          case None => Fail("Session Fail: cannot find constructor of class %s", roleType)
          case Some(constr) => {
            val postIt = ASTUtils.conjuncts(constr.getContract.post_condition, StandardOperator.Star).iterator
            postIt.foreach {
              case op: OperatorExpression => {
                if(op.operator == StandardOperator.Perm) {
                  val permName = op.arg(0) match {
                    case n : NameExpression => n.name
                    case _ => Fail("Session Fail: did not find a name expression as first argument of %s", op.toString); ""
                  }
                  val rolePerm = create.expression(StandardOperator.Perm,create.dereference(create.field_name(roleName),permName),rewrite(op.arg(1)))
                  contr.ensures(rolePerm)
                }
              }
              }
            }
        }
      }
    }
    contr.getContract
  }

  override def visit(m : Method) = {
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind,m.getReturnType,rewrite(m.getContract),getThreadClassName(roleName),m.getArgs,rewrite(m.getBody))
    } else {
      super.visit(m)
    }
  }

  override def visit(d : DeclarationStatement) = {
    if(roleObjects.contains(d)) { // && d.name != roleName
      // remove d
    } else {
      super.visit(d);
    }
  }

  override def visit(a : AssignmentStatement) = {
    getValidNameFromNode(false, a.location) match {
      case Some(otherRole) => {
        if(getValidNameFromExpression(true, a.expression).nonEmpty) { //write a-exp to chan
          val chan = getChanVar(otherRole,true)
          chans += ((roleName,chan.name))
          result = create.invokation(chan, null, chanWrite, a.expression)
        } else {
          // remove a
        }
      }
      case None =>
        getValidNameFromExpression(false, a.expression) match { //receive a-exp at chan
          case Some(n) => {
            val chan = getChanVar(n,false)
            chans += ((roleName,chan.name))
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

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.name(NameExpressionKind.Unresolved, null, getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def getValidNameFromNode(isRole : Boolean, n : ASTNode) : Option[NameExpression] =
    getNameFromNode(n).filter(n => if(isRole) (n.name == roleName) else n.name != roleName)

  private def getValidNameFromExpression(isRole : Boolean, e : ASTNode) : Option[NameExpression] =
    getNamesFromExpression(e).find(n => if(isRole) (n.name == roleName) else n.name != roleName)

}