package vct.col.util

import hre.lang.System.Fail
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelRegion}
import vct.col.ast.stmt.decl.Method.{JavaConstructor, Kind}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, DeclarationStatement, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.util.SessionUtil.{barrierClassName, channelClassName, getNameFromNode, getNamesFromExpression, mainClassName, mainMethodName, runMethodName}
import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}

object SessionStructureCheck {
  def getMainClass(source : ProgramUnit) : ASTClass = source.get().find(_.name == mainClassName).get.asInstanceOf[ASTClass]
  def getRoleNames(source : ProgramUnit) : Iterable[String] =
    getMainClass(source).methods().find(_.kind== Kind.Constructor) //getMainConstructor
      .get.getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement]) //getRoleObjects
      .map(_.location.asInstanceOf[NameExpression].name) //getRoleNames
}

class SessionStructureCheck(source : ProgramUnit) {

  private var mainClass : ASTClass = null
  private var roleObjects : Iterable[AssignmentStatement] = null
  private var roleNames : Iterable[String] = null
  private var roleClasses : Iterable[ASTClass] = null
  private var roleClassNames : Iterable[String] = null
  private var mainMethods : Iterable[Method] = null
  private var mainMethodNames : Iterable[String] = null
  private var pureMainMethodNames : Iterable[String] = null
  private var otherClasses : Iterable[ASTClass] = null

  def check() : Unit = {
    checkMainClass(source)
    mainClass = SessionStructureCheck.getMainClass(source)
    checkMainConstructor()
    checkMainMethod()
    roleNames = getRoleNames()
    roleClasses = getRoleClasses(source)
    roleClassNames = roleClasses.map(_.name)
    mainMethods = getMainMethodsNonPureNonResourcePredicate()
    mainMethodNames = mainMethods.map(_.name)
    pureMainMethodNames = mainClass.methods().filter(_.kind == Method.Kind.Pure).map(_.name)
    checkMainMethodsAllowedSyntax(mainMethods)
    checkMainMethodsRecursion(source)
    checkRoleFieldsTypes(source)
    checkRoleMethodsTypes(source)
    otherClasses = getOtherClasses(source)
    checkOtherClassesFieldsTypes(source)
    checkOtherClassesMethodsTypes(source)
  }

  def getRoleOrHelperClasses() : Iterable[ASTClass] = source.get().filter(c => c.name != mainClassName && c.name != channelClassName && c.name != barrierClassName) .map(_.asInstanceOf[ASTClass])

  private def checkMainClass(source : ProgramUnit) : Unit = {
    source.get().find(_.name == mainClassName) match {
      case None => Fail("Session Fail: class 'Main' is required!")
      case Some(main) =>
        val mcl = main.asInstanceOf[ASTClass]
        val constrs = mcl.methods().filter(_.kind== Kind.Constructor)
        if(constrs.size != 1) {
          Fail("Session Fail: class 'Main' method must have exactly one constructor!")
        } else {
          if (constrs.head.getArity >0){
            Fail("Session Fail: The constructor of class 'Main' cannot have any arguments!")
          } else if(constrs.head.name != mainClassName) {
            Fail("Session Fail: Method without type provided, or constructor with other name than 'Main'")
          }
        }
        mcl.methods().find(_.name == runMethodName) match {
          case None => Fail("Session Fail: The class 'Main' must have a method '%s'!",runMethodName)
          case Some(run) => {
            if (run.getArgs.length != 0)
              Fail("Session Fail: the method '%s' of class 'Main' cannot have any arguments!", runMethodName)
            else if (!isVoidType(run.getReturnType))
              Fail("Session Fail: The return type of method '%s' has to be void!", runMethodName)
          }
        }
        mcl.methods().find(_.name == mainMethodName) match {
          case None => Fail("Session Fail: The class 'Main' must have the following method: \nvoid " + mainMethodName + "() {\n\tMain m = new Main();\n\tm." + runMethodName + "();\n}")
          case Some(mainMethod) =>
            if(mainMethod.getArgs.length != 0)
              Fail("Session Fail: the method '%s' of class 'Main' cannot have any arguments!",mainMethodName)
            else if(!isVoidType(mainMethod.getReturnType))
              Fail("Session Fail: The return type of method '%s' has to be void!", mainMethodName)
        }
    }
  }

  private def getMainConstructor() : Method = mainClass.methods().find(_.kind== Kind.Constructor).get

  def getRoleObjects() : Iterable[AssignmentStatement] = {
    getMainConstructor().getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement])
  }

  private def getRoleNames() : Iterable[String] = roleObjects.map(_.location.asInstanceOf[NameExpression].name)

  private def checkMainConstructor() : Unit  = {
    val roles : Array[ASTNode] = getMainConstructor().getBody match {
      case b: BlockStatement => b.getStatements
      case _ => Fail("Constructor of 'Main' must have a body of type BlockStatement, i.e. be defined!"); Array()
    }
    if(roles.length == 0)
      Fail("Session Fail: Main constructor is mandatory and  must assign at least one role!")
    roles.foreach { r => r match {
      case a: AssignmentStatement => a.location match {
        case n: NameExpression => SessionStructureCheck.getMainClass(source).fields().map(_.name).find(r => r == n.name) match {
          case None => Fail("Session Fail: can only assign to role fields of class 'Main' in constructor")
          case Some(_) => a.expression match {
            case m: MethodInvokation => getRoleOrHelperClasses().find(_.name == m.dispatch.getName) match {
              case None => Fail("Session Fail: Wrong method: constructor of 'Main' must initialize roles with a call to a role constructor")
              case Some(_) => true
            }
            case _ => Fail("Session Fail: No MethodInvokation: constructor of 'Main' must initialize roles with a call to a role constructor")
          }
        }
        case _ => Fail("Session Fail: Can only assign roles, statement %s is not allowed!",a)
      }
      case _ => Fail("Session Fail: constructor of 'Main' can only assign role classes")
    }
    }
    roleObjects = getRoleObjects()
    if(getRoleNames().toSet != mainClass.fields().map(_.name).toSet) {
      Fail("Session Fail: the fields of class 'Main' must be all assigned in constructor 'Main'")
    }
  }

  private def getMainMethodsNonPureNonResourcePredicate() : Iterable[Method] =
    mainClass.methods().filter(m => m.name != mainMethodName && m.kind != Method.Kind.Constructor && m.kind != Method.Kind.Pure && !(m.kind == Method.Kind.Predicate && isResourceType(m.getReturnType)))

  private def isResourceType(t : Type) : Boolean = t match {
    case p : PrimitiveType => p.sort == PrimitiveSort.Resource
    case _ => false
  }

  private def checkMainMethod() : Unit = {
    val mainMethod = mainClass.methods().find(_.name == mainMethodName).get
    mainMethod.getBody match {
      case b : BlockStatement => {
        if(b.getLength != 2)
          Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
        else {
          b.getStatement(1) match {
            case m : MethodInvokation =>
              if(m.method != runMethodName)
                Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
              else m.`object` match {
                case n : NameExpression => //fine
                case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
              }
            case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
          }
          b.getStatement(0) match {
            case v : VariableDeclaration =>
              v.basetype match {
                case c : ClassType => if(c.getName != mainClassName) Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
                case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
              }
              if(v.get().size == 1) {
                  v.get().head match {
                  case d : DeclarationStatement =>
                    d.initJava match {
                      case m : MethodInvokation =>
                        if(!(d.name == b.getStatement(1).asInstanceOf[MethodInvokation].`object`.asInstanceOf[NameExpression].name && m.method == JavaConstructor && m.dispatch.getName == mainClassName))
                          Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
                      case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
                    }
                  case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
                }
              } else Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
            case _ => Fail("Session Fail: Didn't find expected statements\n %s\n in method %s!","\tMain m = new Main();\n\tm." + runMethodName + "();",mainMethodName)
          }
        }
      }
      case _ => Fail("Session Fail: expected BlockStatement for method %s", mainMethodName)
    }

  }

  private def checkMainMethodsAllowedSyntax(methods : Iterable[Method]) : Unit = {
    methods.foreach(m => checkMainStatement(m.getBody))
  }

  private def checkMainStatement(s : ASTNode) : Unit = {
    s match {
      case b : BlockStatement => b.getStatements.foreach(checkMainStatement)
      case a: AssignmentStatement =>
        a.location match {
          case n : NameExpression => if(roleNames.contains(n.name)) Fail("Session Fail: cannot assign role anywhere else then in Main constructor")
          case _ => //fine, continue check
        }
        getNameFromNode(a.location).map(_.name) match {
          case Some(n) => if (!roleNames.contains(n)) Fail("Session Fail: the assignment %s has a non-role name in its location.",a.toString)
          case None => Fail("Session Fail: the assignment %s in a method of class 'Main' must have one role in its location.",a.toString)
        }
        val expNames = getNamesFromExpression(a.expression).map(_.name).toSet.filter(roleNames.contains(_))
        if(expNames.size > 1) {
          Fail("Session Fail: the assignment %s in a method of class 'Main' cannot have multiple roles in its expression.",a.toString)
        }
        val mi = getMethodInvocationsFromExpression(a.expression)
        if(mi.exists(m => m.method == Method.JavaConstructor && (m.dispatch.getName == mainClassName || roleClassNames.contains(m.dispatch.getName))))
          Fail("Session Fail: Cannot assign a new Main or role object in statement %s! %s", a.toString,a.getOrigin)
      case i: IfStatement => {
        if (i.getCount == 1 || i.getCount == 2) {
          if (checkSessionCondition(i.getGuard(0), roleNames)) {
            checkMainStatement(i.getStatement(0))
            if (i.getCount == 2) checkMainStatement(i.getStatement(1))
          } else Fail("Session Fail: IfStatement needs to have one condition for each role! " + s.getOrigin)
        } else Fail("Session Fail: one or two branches expected in IfStatement! " + s.getOrigin)
      }
      case l: LoopStatement => {
        if (l.getInitBlock == null && l.getUpdateBlock == null) //it is a while loop
          if (checkSessionCondition(l.getEntryGuard, roleNames))
            checkMainStatement(l.getBody)
          else Fail("Session Fail: a while loop needs to have one condition for each role! " + s.getOrigin)
        else Fail("Session Fail: a for loop is not supported, use a while loop! " + s.getOrigin)
      }
      case p : ParallelRegion => {
        p.blocks.foreach(b => checkMainStatement(b.block))
      }
      case m : MethodInvokation =>
        if(m.method == mainClassName)
          Fail("This should have been detected by typechecker: cannot call method '%s'!",mainClassName)
        else if(m.method == Method.JavaConstructor && m.dispatch.getName == mainClassName)
          Fail("Session Fail: cannot call constructor '%s'!",mainClassName)
        else if(m.method == Method.JavaConstructor && roleClassNames.contains(m.dispatch.getName))
            Fail("Session Fail: cannot call role constructor '%s'",m.dispatch.getName)
        else if(pureMainMethodNames.contains(m.method))
          Fail("Session Fail: cannot have a method call statement for pure method '%s'! %s",m.method,m.getOrigin)
        else if(!mainMethodNames.contains(m.method)) { //it is a role or other class method
          if(m.`object` == null) {
            Fail("Session Fail: method call not allowed or object of method call '%s' is not given! %s",m.method,m.getOrigin)
          }
          m.`object` match {
            case n : NameExpression =>
              if(!roleNames.contains(n.name))
                Fail("Session Fail: invocation of method %s is not allowed here, because method is either pure, or from a non-role class! %s",m.method, m.getOrigin)
          }
          val roles = getNamesFromExpression(m).filter(n => roleNames.contains(n.name))
          if(roles.size > 1)
            Fail("Session Fail: Non-Main method call %s uses object and/or arguments from multiple roles! %s",m.toString,m.getOrigin)
        }
      case as : ASTSpecial =>
        if(as.kind != ASTSpecial.Kind.Fold || as.kind != ASTSpecial.Kind.Unfold)
          Fail("Session Fail: Syntax not allowed; statement is not a session statement! " + s.getOrigin)
      case _ => Fail("Session Fail: Syntax not allowed; statement is not a session statement! " + s.getOrigin)
    }
  }

  private def checkSessionCondition(node: ASTNode, roleNames : Iterable[String]) : Boolean = {
    val roles = splitOnAnd(node).map(getNamesFromExpression).map(_.map(_.name).filter(roleNames.contains(_)).toSet)
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

  private def getMethodInvocationsFromExpression(e : ASTNode): Set[MethodInvokation] = {
    e match {
      case o : OperatorExpression => o.args.flatMap(getMethodInvocationsFromExpression(_)).toSet
      case m : MethodInvokation => m.args.flatMap(getMethodInvocationsFromExpression(_)).toSet + m
      case _ => Set.empty
    }
  }

  private def getRoleClasses(source : ProgramUnit) : Iterable[ASTClass] = {
    val roleClassTypes = roleObjects.map(_.expression.asInstanceOf[MethodInvokation].dispatch.getName)
    getRoleOrHelperClasses().filter(c => roleClassTypes.contains(c.name))
  }

  private def checkMainMethodsRecursion(source : ProgramUnit) : Unit = {
    mainMethods.foreach(m => checkGuardedRecursion(m.getBody,Set(m.name)))
  }


  private def checkGuardedRecursion(statement : ASTNode, encounteredMethods : Set[String]) : Unit =
    statement match {
      case b : BlockStatement => if(b.getLength > 0) checkGuardedRecursion(b.getStatement(0), encounteredMethods)
      case i : MethodInvokation =>
        if(encounteredMethods.contains(i.method))
          Fail("Session Fail: recursive call not allowed as first statement of method '%s'! %s", i.method, statement.getOrigin)
        else mainMethods.find(_.name == i.method) match {
          case Some(m) => checkGuardedRecursion(m.getBody,encounteredMethods + m.name)
          case None => //fine, it is a pure main method, role class or other class method (without any recursion)
        }
      case i : IfStatement => {
        checkGuardedRecursion(i.getStatement(0), encounteredMethods)
        if (i.getCount == 2)
          checkGuardedRecursion(i.getStatement(1), encounteredMethods)
      }
      case l : LoopStatement => checkGuardedRecursion(l.getBody, encounteredMethods)
      case p : ParallelRegion => p.blocks.foreach(b => checkGuardedRecursion(b.block,encounteredMethods))
      case a : AssignmentStatement => checkGuardedRecursion(a.expression,encounteredMethods)
      case e : OperatorExpression => e.args.foreach(checkGuardedRecursion(_,encounteredMethods))
      case _ => //fine
    }

  private def checkRoleMethodsTypes(source : ProgramUnit) : Unit = {
    roleClasses.foreach(_.methods().forEach(checkRoleMethodTypes(_)))
  }

  private def checkRoleMethodTypes(roleMethod : Method) : Unit = {
    if(!isNonRoleOrPrimitive(roleMethod.getReturnType,true,false)) {
      Fail("Session Fail: return type of method %s is a role or other unexpected type",roleMethod.name)
    }
    roleMethod.getArgs.foreach(arg => {
      if(!isNonRoleOrPrimitive(arg.`type`,false,roleMethod.kind == Method.Kind.Pure)) {
        Fail("Session Fail: the type of argument %s of method %s is a role or other unexpected type",arg.name,roleMethod.name)
      }
    })
  }

  private def checkRoleFieldsTypes(source : ProgramUnit) : Unit = {
    roleClasses.foreach(role => role.fields().foreach(field => {
     if(!isNonRoleOrPrimitive(field.`type`,false,false))
       Fail("Session Fail: type '%s' of field '%s' of role '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def isNonRoleOrPrimitive(t : Type, isVoid : Boolean, allowRoles : Boolean) : Boolean =
    isBasePrimitiveType(t, allowRoles) || isOptionOfArray(t, allowRoles) || isSequence(t, allowRoles) || isVoid && isVoidType(t)

  def isVoidType(a : ASTNode) = a match {
    case p : PrimitiveType => p.isVoid
    case _ => false
  }

  private def isBasePrimitiveType(a : ASTNode, allowRoles : Boolean) = a match {
    case p : PrimitiveType => isBaseType(p)
    case c : ClassType => c.getName != mainClassName && c.getName != barrierClassName && c.getName != channelClassName && (allowRoles || !roleClassNames.contains(c.getName))
    case _ => false
  }

  private def isBaseType(p : PrimitiveType) = p.isBoolean || p.isDouble || p.isInteger || p.sort == PrimitiveSort.Resource

  private def isOptionOfArray(o : ASTNode, allowRoles : Boolean) = o match {
    case p : PrimitiveType => p.sort match {
        case PrimitiveSort.Option => p.nrOfArguments == 1 && isArray(p.args.head, allowRoles)
        case _ => false
    }
    case _ => false
  }

  private def isArray(a : ASTNode, allowRoles : Boolean) : Boolean = a match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Array => p.nrOfArguments == 1 && (isCell(p.args.head, allowRoles) || isArray(p.args.head, allowRoles))
      case _ => false
    }
    case _ => false
  }

  private def isCell(c : ASTNode, allowRoles : Boolean) = c match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Cell => p.nrOfArguments == 1 && isBasePrimitiveType(p.args.head, allowRoles)
      case _ => false
    }
    case _ => false
  }

  private def isSequence(s : ASTNode, allowRoles : Boolean) : Boolean = s match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Sequence => p.nrOfArguments == 1 && (isBasePrimitiveType(p.args.head, allowRoles) || isSequence(p.args.head, allowRoles))
      case _ => false
    }
    case _ => false
  }

  private def getOtherClasses(source : ProgramUnit) : Iterable[ASTClass] = {
    source.get().filter({
      case c : ASTClass => c.name != mainClassName && !roleClassNames.contains(c.name) && c.name != channelClassName && c.name != barrierClassName
    }).map(_.asInstanceOf[ASTClass])
  }

  private def checkOtherClassesFieldsTypes(source : ProgramUnit) : Unit = {
    otherClasses.foreach(role => role.fields().foreach(field => {
      if(!isNonRoleOrPrimitive(field.`type`,false,false))
        Fail("Session Fail: type '%s' of field '%s' of non-role class '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def checkOtherClassesMethodsTypes(source: ProgramUnit) : Unit = {
    otherClasses.foreach(_.methods().forEach(checkRoleMethodTypes(_)))
  }

}
