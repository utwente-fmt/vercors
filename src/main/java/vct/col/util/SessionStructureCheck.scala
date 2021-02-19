package vct.col.util

import hre.lang.System.Fail
import vct.col.ast.`type`.{ClassType, PrimitiveType, Type}
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelRegion}
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.util.SessionUtil.{barrierClassName, channelClassName, getNamesFromExpression, isThreadClassName, mainClassName, runMethodName}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object SessionStructureCheck {

  def check(source : ProgramUnit) : Unit = {
    checkNonMainClasses(source)
    checkMainClass(source)
    checkRoleObjectsInMainConstructor(source)
    checkMainMethodsAllowedSyntax(source)
    checkMainMethodsRecursion(source)
    checkRoleFieldsTypes(source)
    checkRoleMethodsTypes(source)
    checkOtherClassesFieldsTypes(source)
    checkOtherClassesMethodsTypes(source)
    checkAbsenceRecursionNonMain(source)
    checkLoopAbsenceNonMain(source)
  }

  def getNonMainClasses(source : ProgramUnit) : Iterable[ASTClass] = source.get().filter(_.name != mainClassName) .map(_.asInstanceOf[ASTClass])

  private def checkNonMainClasses(source : ProgramUnit) : Unit = {
    if (getNonMainClasses(source).isEmpty)
      Fail("Session Fail: A session program needs to have at least one class representing a role")
  }

  def getMainClass(source : ProgramUnit) : ASTClass = source.get().find(_.name == mainClassName).get.asInstanceOf[ASTClass]

  private def checkMainClass(source : ProgramUnit) : Unit = {
    source.get().find(_.name == mainClassName) match {
      case None => Fail("Session Fail: class 'Main' is required")
      case Some(main) =>
        val mcl = main.asInstanceOf[ASTClass]
        val constrs = mcl.methods().filter(_.kind== Kind.Constructor)
        if(constrs.size != 1) {
          Fail("Session Fail: class 'Main' method must have exactly one constructor")
        } else {
          if (constrs.head.getArity >0){
            Fail("Session Fail: The constructor of class 'Main' cannot have any arguments")
          }
        }
        mcl.methods().find(_.name == runMethodName) match {
          case None => Fail("Session Fail: The class 'Main' must have a method '%s'",runMethodName)
          case Some(run) => if(run.getArgs.length != 0) Fail("Session Fail: the method %s of class 'Main' cannot have any arguments",runMethodName)
        }
    }
  }

  def getMainConstructor(source : ProgramUnit) : Method = getMainClass(source).methods().find(_.kind== Kind.Constructor).get

  def getRoleObjects(source : ProgramUnit) : Array[AssignmentStatement] = {
    getMainClass(source).methods().find(_.kind == Method.Kind.Constructor).get.getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement])
  }

  def getRoleObjectNames(source : ProgramUnit) : Array[String] = getRoleObjects(source).map(_.location.asInstanceOf[NameExpression].name)

  private def checkRoleObjectsInMainConstructor(source : ProgramUnit) : Unit  = {
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
    if(getRoleObjectNames(source).toSet != getMainClass(source).fields().map(_.name).toSet) {
      Fail("Session Fail: the fields of class 'Main' must be the roles")
    }
  }

  def getMainMethods(source : ProgramUnit) : Iterable[Method] = getMainClass(source).methods().filter(m => m.kind != Method.Kind.Constructor && m.kind != Method.Kind.Pure)

  private def checkMainMethodsAllowedSyntax(source : ProgramUnit) : Unit = {
    val roleNames = getRoleObjectNames(source)
    val mainMethods = getMainMethods(source)
    val mainMethodNames = mainMethods.map(_.name)
    getMainMethods(source).foreach(m => checkMainStatement(m.getBody,roleNames, mainMethodNames))
  }

  private def checkMainStatement(s : ASTNode, roleNames : Array[String], mainMethodNames : Iterable[String]) : Unit = { //tau nog toestaan hier
    s match {
      case b : BlockStatement => b.getStatements.foreach(checkMainStatement(_,roleNames,mainMethodNames))
      case a: AssignmentStatement =>
        val expNames = getNamesFromExpression(a.expression).map(_.name).toSet.filter(roleNames.contains(_))
        if(expNames.size > 1) {
          Fail("Session Fail: the assignment %s in a method of class 'Main' cannot have multiple roles in its expression.",a.toString)
        }
      case i: IfStatement =>
        if(i.getCount == 1 || i.getCount == 2) {
          if (checkSessionCondition(i.getGuard(0), roleNames)) {
            checkMainStatement(i.getStatement(0), roleNames,mainMethodNames)
            if(i.getCount == 2) checkMainStatement(i.getStatement(1), roleNames,mainMethodNames)
          } else Fail("Session Fail: a while loop needs to have one condition for each role! " + s.getOrigin)
        } else Fail("Session Fail: one or two branches expected in IfStatement " + s.getOrigin)
      case l: LoopStatement => {
        if (l.getInitBlock == null && l.getUpdateBlock == null)
          if (checkSessionCondition(l.getEntryGuard, roleNames))
            checkMainStatement(l.getBody, roleNames,mainMethodNames)
          else Fail("Session Fail: a while loop needs to have one condition for each role! " + s.getOrigin)
        else Fail("Session Fail: a for loop is not supported, use a while loop " + s.getOrigin)
      }
      case p : ParallelRegion => {
        p.blocks.foreach(b => checkMainStatement(b.block,roleNames,mainMethodNames))
      }
      case m : MethodInvokation =>
        if(m.method == mainClassName)
          Fail("Session Fail: cannot call constructor '%s'!",mainClassName)
        else if(roleNames.contains(m.method))
          Fail("Session Fail: cannot call role constructor '%s'",m.method)
        else if(!mainMethodNames.contains(m.method))
          m.`object` match {
            case n : NameExpression =>
              if(!roleNames.contains(n.name))
                Fail("Session Fail: invocation of method %s is not allowed here, because method is either pure, or from a non-role class!" + m.getOrigin)
          }
      case _ => Fail("Session Fail: Syntax not allowed; statement is not a session statement! " + s.getOrigin)
    }
  }

  private def checkSessionCondition(node: ASTNode, roleNames : Array[String]) : Boolean = {
    val roles = splitOnAnd(node).map(getNamesFromExpression).map(_.map(_.name).toSet)
    roles.forall(_.size == 1) && roleNames.toSet == roles.flatten
  }

  private def splitOnAnd(node : ASTNode) : Set[ASTNode] = {
    node match {
      case e : OperatorExpression => e.operator match {
        case StandardOperator.And => splitOnAnd(e.first) ++ splitOnAnd(e.second)
        case _ => Set(node)
      }
      case _ => Set(node)
    }
  }

  def getRoleClasses(source : ProgramUnit) : Iterable[ASTClass] = {
    val roleClassNames = getRoleObjects(source).map(_.expression.asInstanceOf[MethodInvokation].dispatch.getName)
    getNonMainClasses(source).filter(c => roleClassNames.contains(c.name))
  }

  private def checkMainMethodsRecursion(source : ProgramUnit) : Unit = {
    val mainMethods = getMainMethods(source)
    mainMethods.foreach(m => checkGuardedRecursion(m.getBody,Set(m.name),mainMethods))
  }

  private def checkAbsenceRecursionNonMain(source : ProgramUnit) : Unit =
    source.get().filter({
      case c : ASTClass => c.name != mainClassName
      case _ => false
    }).map(_.asInstanceOf[ASTClass]).foreach(c => {
      val classMethods = c.methods()
      classMethods.foreach(m => checkAbsenceRecursion(m.getBody,Set(m.name),classMethods))
    })

  private def checkGuardedRecursion(statement : ASTNode, encounteredMethods : Set[String], mainMethods : Iterable[Method]) : Unit =
    statement match {
      case b : BlockStatement => checkGuardedRecursion(b.getStatement(0), encounteredMethods,mainMethods)
      case i : MethodInvokation =>
        if(encounteredMethods.contains(i.method))
          Fail("Session Fail: recursive call not allowed as first statement of method '%s'! %s", i.method, statement.getOrigin)
        else mainMethods.find(_.name == i.method) match {
          case Some(m) => checkGuardedRecursion(m.getBody,encounteredMethods + m.name,mainMethods)
          case None => //fine, it is a role method (without any recursion)
        }
      case _ => checkRecursionEasyNodeClasses(statement,encounteredMethods, (s, e) => checkGuardedRecursion(s,e,mainMethods))
    }

  private def checkAbsenceRecursion(statement : ASTNode, encounteredMethods : Set[String], methodDefs : Iterable[Method]) : Unit =
    statement match {
      case b : BlockStatement => b.getStatements.foreach(s => checkAbsenceRecursion(s,encounteredMethods,methodDefs))
      case m : MethodInvokation =>
        if(encounteredMethods.contains(m.method))
          Fail("Session Fail: role methods are not allowed to use recursion; recursion encountered for method '%s' at %s", m.method, m.getOrigin)
        else methodDefs.find(_.name == m.method) match {
          case Some(method) => checkAbsenceRecursion(method.getBody,encounteredMethods + m.method, methodDefs)
          case None => Fail("Session Fail: Wrong method invocation, method %s is from another class! %s", m.method, m.getOrigin)
        }
      case _ => checkRecursionEasyNodeClasses(statement,encounteredMethods, (s, e) => checkAbsenceRecursion(s,e,methodDefs))
    }

  private def checkRecursionEasyNodeClasses(statement : ASTNode, encountered : Set[String], check : (ASTNode , Set[String]) => Unit) : Unit =
    statement match {
      case i : IfStatement => {
        check(i.getStatement(0), encountered)
        if (i.getCount == 2)
          check(i.getStatement(1), encountered)
      }
      case l : LoopStatement => check(l.getBody, encountered)
      case p : ParallelRegion => p.blocks.foreach(b => check(b.block,encountered))
      case a : AssignmentStatement => check(a.expression,encountered)
      case e : OperatorExpression => e.args.foreach(check(_,encountered))
      case _ => //fine!
    }

  private def checkRoleMethodsTypes(source : ProgramUnit) : Unit = {
    val roles = getRoleClasses(source)
    val roleClassNames = roles.map(_.name)
    roles.foreach(_.methods().forEach(checkRoleMethodTypes(_,roleClassNames)))
  }

  private def checkRoleMethodTypes(roleMethod : Method, roleClassNames : Iterable[String]) : Unit = {
    if(!isNonRoleOrPrimitive(roleMethod.getReturnType,roleClassNames)) {
      Fail("Session Fail: return type of method %s is a role or other unexpected type",roleMethod.name)
    }
    roleMethod.getArgs.foreach(arg => {
      if(!isNonRoleOrPrimitive(arg.`type`, roleClassNames)) {
        Fail("Session Fail: the type of argument %s of method %s is a role or other unexpected type",arg.name,roleMethod.name)
      }
    })
  }

  private def checkRoleFieldsTypes(source : ProgramUnit) : Unit = {
    val roles = getRoleClasses(source)
    val roleClassNames = roles.map(_.name)
    roles.foreach(role => role.fields().foreach(field => {
     if(!isNonRoleOrPrimitive(field.`type`,roleClassNames))
       Fail("Session Fail: type '%s' of field '%s' of role '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def isNonRoleOrPrimitive(t : Type, roleClassNames : Iterable[String]) : Boolean = t match {
    case p : PrimitiveType => p.isBoolean || p.isDouble || p.isInteger || p.isVoid
    case c : ClassType => c.getName != mainClassName && !roleClassNames.contains(c.getName)
    case _ => Fail("Session Fail: didn't expect this Type: " + t.toString); false
  }

  private def getOtherClasses(source : ProgramUnit) : Iterable[ASTClass] = {
    val roleClassNames = getRoleClasses(source).map(_.name)
    source.get().filter({
      case c : ASTClass => c.name != mainClassName && !roleClassNames.contains(c.name) && c.name != channelClassName && c.name != barrierClassName
    }).map(_.asInstanceOf[ASTClass])
  }

  private def checkOtherClassesFieldsTypes(source : ProgramUnit) : Unit = {
    val others = getOtherClasses(source)
    others.foreach(role => role.fields().foreach(field => {
      if(!isNonRoleOrPrimitive(field.`type`,Set()))
        Fail("Session Fail: type '%s' of field '%s' of non-role class '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def checkOtherClassesMethodsTypes(source: ProgramUnit) : Unit = {
    val others = getOtherClasses(source)
    others.foreach(_.methods().forEach(checkRoleMethodTypes(_,Set())))
  }

  private def checkLoopAbsenceNonMain(source : ProgramUnit) =
    getNonMainClasses(source).foreach(_.methods().foreach(m => checkAbsenceOfLoops(m.getBody)))


  private def checkAbsenceOfLoops(statement : ASTNode) : Unit = { statement match {
      case l : LoopStatement => Fail("Session Fail: loop not allowed in method of non-Main class")
      case b : BlockStatement => b.getStatements.foreach(checkAbsenceOfLoops)
      case i : IfStatement => {
        checkAbsenceOfLoops(i.getStatement(0))
        if (i.getCount == 2)
          checkAbsenceOfLoops(i.getStatement(1))
      }
      case p : ParallelRegion => p.blocks.foreach(b => checkAbsenceOfLoops(b.block))
      case _ => //fine!
    }
  }

}
