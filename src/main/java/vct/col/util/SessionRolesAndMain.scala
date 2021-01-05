package vct.col.util

import hre.lang.System.Fail
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.MethodInvokation
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.util.SessionUtil.{getThreadClassName, mainClassName, runMethodName}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class SessionRolesAndMain(val source: ProgramUnit) {
  val roleClasses : Iterable[ASTClass] = getRolesClasses(source)
  val mainClass : ASTClass = getMainClass(source)
  val mainMethod : Method = getMainMethod()
  val roleObjects : List[DeclarationStatement] = getRoleObjects()

  def getRoleName(c : ASTClass) : Option[String] =
    roleObjects.find(role => c.name == getThreadClassName(role.name)).map(_.name)

  private def getRolesClasses(source : ProgramUnit): Iterable[ASTClass] = {
    val roles = source.get().filter {
      case c: ASTClass => c.name != mainClassName
      case _ => false
    }.map(_.asInstanceOf[ASTClass])
    if (roles.isEmpty)
      Fail("A session type needs to have at least one class representing a role")
    roles
  }

  private def getMainClass(source : ProgramUnit) : ASTClass = {
    val main = source.get().find {
      case c: ASTClass => c.getName == mainClassName
      case _ => false
    }
    val m = source.get().map(_.asInstanceOf[ASTClass].getName)
    val b = m.contains("Main")
    val btrue = b;
    if(main.isEmpty)
      Fail("Session type must have a class 'Main'")
    val mcl = main.head.asInstanceOf[ASTClass]
    for(method <- mcl.methods()) {
      if(method.kind== Kind.Constructor) {
        if (method.getArity >0){
          Fail("The constructor of class 'Main' cannot have any arguments")
        } else {
          if(method.getBody match {
            case b : BlockStatement => b.getStatements.nonEmpty
            case _ => true
          }){
            Fail("The constructor of class 'Main' must have no body")
          }
        }
      }
    }
    mcl
  }
  private def getMainMethod() : Method = {
    val mains = mainClass.methods().filter(m => m.getName == runMethodName)
    if(mains.isEmpty)
      Fail("Main class does not have a method 'main'.")
    else if(mains.size > 1)
      Fail("Main class has multiple methods 'main' while only one is allowed.")
    mains.head
  }

  private def getRoleObjects() : List[DeclarationStatement] = {
    val arrayRoles = if(mainMethod.getArgs.length > 0) getRolesFromMainArguments() else getRolesFromMainBody()
    if(arrayRoles.isEmpty)
      Fail("Method 'main' does not declare roles in its arguments or its first statements.")
    arrayRoles.toList.asInstanceOf[List[DeclarationStatement]]
  }

  private def getRolesFromMainArguments() : Array[DeclarationStatement] = {
    val roleArgs = mainMethod.getArgs.filter(arg => roleClasses.exists(_.getName == arg.getDeclName.toString))
    val roleSeq =
      if (mainMethod.getArity == 1 && isSequenceOfRoles(mainMethod.getArgs.head))
          mainMethod.getArgs
      else Array() : Array[DeclarationStatement]
    if (roleArgs.nonEmpty) roleArgs else roleSeq
  }

  private def isSequenceOfRoles(arg : DeclarationStatement) : Boolean = {
    val isSequence = arg.`type` match {
      case p : PrimitiveType => p.sort == PrimitiveSort.Sequence
      case _ => false
    }
    val isRole = if (arg.`type`.args.length == 1)
                  typeIsRoleClass(arg.getType.args(0).getType)
                 else false
    isSequence && isRole
  }

  private def getRolesFromMainBody() : Array[ASTNode] = {
    mainMethod.getBody match {
      case b: BlockStatement =>
        b.getStatements.takeWhile {
          case d: DeclarationStatement => d.init match {
            case Some(i) => i match {
              case mi: MethodInvokation => mi.method.equals(Method.JavaConstructor) && typeIsRoleClass(d.getType)
              case _ => false
            }
            case _ => false
          }
          case _ => false
        }
      case _ => Array()
    }
  }

  private def typeIsRoleClass(t : Type) : Boolean = {
    t match {
      case ct : ClassType => roleClasses.exists(_.getName.equals(ct.getName))
      case _ => false
    }
  }

}

