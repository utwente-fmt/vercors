package vct.col.util

import hre.lang.System.Fail
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveType, Type}
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionUtil.{channelClassName, isThreadClassName, mainClassName, runMethodName}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object SessionStructureCheck {


  def check(source : ProgramUnit) : Unit = {
    checkNonMainClasses(source)
    checkMainClass(source)
    checkRoleObjectsInMainConstructor(source)
    checkRoleMethodsTypes(source)
  }

  def getNonMainClasses(source : ProgramUnit) = source.get().filter(_.name != mainClassName) .map(_.asInstanceOf[ASTClass])

  private def checkNonMainClasses(source : ProgramUnit) = {
    if (getNonMainClasses(source).isEmpty)
      Fail("Session Fail: A session program needs to have at least one class representing a role")
  }

  def getMainClass(source : ProgramUnit) : ASTClass = source.get().find(_.name == mainClassName).get.asInstanceOf[ASTClass]

  private def checkMainClass(source : ProgramUnit) = {
    source.get().find(_.name == mainClassName) match {
      case None => Fail("Session Fail: class 'Main' is required")
      case Some(main) => {
        val mcl = main.asInstanceOf[ASTClass]
        val constrs = mcl.methods().filter(_.kind== Kind.Constructor)
        if(constrs.size != 1) {
          Fail("Session Fail: class 'Main' method must have exactly one constructor")
        } else {
          if (constrs.head.getArity >0){
            Fail("The constructor of class 'Main' cannot have any arguments")
          }
        }
        if(!mcl.methods().exists(_.name == runMethodName)) {
          Fail("The class 'Main' must have a method '%s'",runMethodName)
        }
      }
    }
  }

  def getMainConstructor(source : ProgramUnit) = getMainClass(source).methods().find(_.kind== Kind.Constructor).get

  def getMainRunMethod(source : ProgramUnit) = getMainClass(source).methods().find(_.name == runMethodName).get

  def getRoleObjects(source : ProgramUnit) : Array[AssignmentStatement] = {
    getMainClass(source).methods().find(_.kind == Method.Kind.Constructor).get.getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement])
  }

  def getRoleObjectNames(source : ProgramUnit) : Array[String] = getRoleObjects(source).map(_.location.asInstanceOf[NameExpression].name)

  private def checkRoleObjectsInMainConstructor(source : ProgramUnit)  = {
    val roles : Array[ASTNode] = getMainConstructor(source).getBody match {
      case b: BlockStatement => b.getStatements
      case _ => Fail("Constructor of 'Main' must have a body of type BlockStatement"); Array()
    }
    roles.foreach {
      case a: AssignmentStatement => a.location match {
        case n: NameExpression => getMainClass(source).fields().map(_.name).find(r => r == n.name) match {
          case None => Fail("Session Fail: can only assign to role fields of class 'Main' in constructor")
          case Some(_) => a.expression match {
            case m: MethodInvokation => getNonMainClasses(source).filter(_.name != channelClassName).find(_.name == m.dispatch.getName) match {
              case None => Fail("Session Fail: Wrong method: constructor of 'Main' must initialize roles with a call to a role constructor")
              case Some(_) => true
            }
            case _ => Fail("Session Fail: No MethodInvokation: constructor of 'Main' must initialize roles with a call to a role constructor")
          }
        }
      }
      case _ => Fail("Session Fail: constructor of 'Main' can only assign role classes")
    }
  }

  def getRoleClasses(source : ProgramUnit) : Iterable[ASTClass] = {
    val roleClassNames = getRoleObjects(source).map(_.expression.asInstanceOf[MethodInvokation].dispatch.getName)
    getNonMainClasses(source).filter(c => roleClassNames.contains(c.name))
  }

  private def checkRoleMethodsTypes(source : ProgramUnit) = {
    val roles = getRoleClasses(source)
    val roleClassNames = roles.map(_.name)
    roles.foreach(_.methods().forEach(checkRoleMethodTypes(_,roleClassNames)))
  }

  private def checkRoleMethodTypes(roleMethod : Method, roleClassNames : Iterable[String]) = {
    if(!isNonRoleOrPrimitive(roleMethod.getReturnType,roleClassNames)) {
      Fail("Session Fail: return type of method %s is a role or other unexpected type",roleMethod.name)
    }
    roleMethod.getArgs.foreach(arg => {
      if(!isNonRoleOrPrimitive(arg.`type`, roleClassNames)) {
        Fail("Session Fail: the type of argument %s of method %s is a role or other unexpected type",arg.name,roleMethod.name)
      }
    })
  }

  private def isNonRoleOrPrimitive(t : Type, roleClassNames : Iterable[String]) : Boolean = t match {
    case p : PrimitiveType => p.isBoolean || p.isDouble || p.isInteger || p.isVoid
    case c : ClassType => c.getName != mainClassName && !roleClassNames.contains(c.getName)
    case _ => Fail("Session Fail: didn't expect this type of Type"); false
  }
}
