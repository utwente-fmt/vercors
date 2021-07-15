package vct.col.veymont

import hre.lang.System.Fail
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelRegion}
import vct.col.ast.stmt.decl.Method.{JavaConstructor, Kind}
import vct.col.ast.stmt.decl._
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.ASTUtils
import Util._
import vct.col.veymont.StructureCheck.{fixedMainFail, fixedMainMethod, getRoleOrHelperClass, isRealResource}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object StructureCheck {
  def getMainClass(source : ProgramUnit) : ASTClass = source.get().asScala.find(_.name == mainClassName).get.asInstanceOf[ASTClass]
  def getRoleOrHelperClass(source : ProgramUnit): Iterable[ASTClass] =
    source.get().asScala.filter(c => c.isInstanceOf[ASTClass] && isRoleOrHelperClassName(c.name)).map(_.asInstanceOf[ASTClass])
  def getRoleNames(source : ProgramUnit) : Iterable[String] =
    getMainClass(source).methods().asScala.find(_.kind== Kind.Constructor) //getMainConstructor
      .get.getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement]) //getRoleObjects
      .map(_.location.asInstanceOf[NameExpression].name) //getRoleNames

  def isExecutableMainMethod(m : Method) : Boolean =
    m.name != mainMethodName &&
      m.kind != Method.Kind.Constructor &&
      m.kind != Method.Kind.Pure &&
      !isRealResource(m)

  private val fixedMainMethodBody : String =  "\tMain m = new Main();\n\tm." + runMethodName + "();"
  private val fixedMainMethod : String  = "\nvoid " + mainMethodName + "() {\n" + fixedMainMethodBody + "\n}"
  private def fixedMainFail(): Unit = Fail("VeyMont Fail: Didn't find expected statements\n %s\n in method %s!",fixedMainMethodBody,mainMethodName)

  private def isResourceType(t : Type) : Boolean = t match {
    case p : PrimitiveType => p.sort == PrimitiveSort.Resource
    case _ => false
  }
  private def isRealResource(m : Method) = m.kind == Method.Kind.Predicate && isResourceType(m.getReturnType)

  def isAllowedPrimitive(p : PrimitiveType): Boolean = p.isBoolean || p.isDouble || p.isInteger
}

class StructureCheck(source : ProgramUnit) {
  checkMainClass(source)
  private val mainClass : ASTClass = StructureCheck.getMainClass(source)
  checkMainConstructor()
  private val roleObjects : Iterable[AssignmentStatement] = getRoleObjects
  checkRoleNames()
  checkMainMethod()
  private val roleNames : Iterable[String] = getRoleNames
  private val roleClasses : Iterable[ASTClass] = getRoleClasses
  private val roleClassNames : Iterable[String] = roleClasses.map(_.name)
  private val mainMethods : Iterable[Method] = getMainMethodsNonPureNonResourcePredicate
  private val mainMethodNames : Iterable[String] = mainMethods.map(_.name)
  private val nonPlainMainMethodNames : Iterable[String] =
    mainClass.methods().asScala.filter(m => m.kind == Method.Kind.Pure || m.kind == Method.Kind.Predicate).map(_.name)
  private var prevAssertArg : ASTNode = null
  checkMainMethodsAllowedSyntax(mainMethods)
  checkRoleFieldsTypes(source)
  checkRoleMethodsTypes(source)
  private val otherClasses : Iterable[ASTClass] = getOtherClasses(source)
  checkOtherClassesFieldsTypes()
  checkOtherClassesMethodsTypes()

  private def checkMainClass(source : ProgramUnit) : Unit = {
    source.get().asScala.find(_.name == mainClassName) match {
      case None => Fail("VeyMont Fail: class 'Main' is required!")
      case Some(main) =>
        val mcl = main.asInstanceOf[ASTClass]
        val constrs = mcl.methods().asScala.filter(_.kind== Kind.Constructor)
        if(constrs.size != 1) {
          Fail("VeyMont Fail: class 'Main' method must have exactly one constructor!")
        } else {
          if (constrs.head.getArity >0){
            Fail("VeyMont Fail: The constructor of class 'Main' cannot have any arguments!")
          } else if(constrs.head.name != mainClassName) {
            Fail("VeyMont Fail: Method without type provided, or constructor with other name than 'Main'")
          } else { //all fine
          }
        }
        mcl.methods().asScala.find(_.name == runMethodName) match {
          case None => Fail("VeyMont Fail: The class 'Main' must have a method '%s'!",runMethodName)
          case Some(run) => {
            if (run.getArgs.length != 0)
              Fail("VeyMont Fail: the method '%s' of class 'Main' cannot have any arguments!", runMethodName)
            else if (!isVoidType(run.getReturnType))
              Fail("VeyMont Fail: The return type of method '%s' has to be void!", runMethodName)
            else { //all fine
            }
          }
        }
        mcl.methods().asScala.find(_.name == mainMethodName) match {
          case None => Fail("VeyMont Fail: The class 'Main' must have the following method: " + fixedMainMethod)
          case Some(mainMethod) =>
            if(mainMethod.getArgs.length != 0)
              Fail("VeyMont Fail: the method '%s' of class 'Main' cannot have any arguments!",mainMethodName)
            else if(!isVoidType(mainMethod.getReturnType))
              Fail("VeyMont Fail: The return type of method '%s' has to be void!", mainMethodName)
            else { //all fine
            }
        }
    }
  }

  private def getMainConstructor : Method = mainClass.methods().asScala.find(_.kind== Kind.Constructor).get

  def getRoleObjects : Iterable[AssignmentStatement] = {
    getMainConstructor.getBody.asInstanceOf[BlockStatement].getStatements.map(_.asInstanceOf[AssignmentStatement])
  }

  private def getRoleNames : Iterable[String] = roleObjects.map(_.location.asInstanceOf[NameExpression].name)

  private def checkMainConstructor() : Unit  = {
    val roles : Array[ASTNode] = getBlockOrThrow(getMainConstructor.getBody,
      "Constructor of 'Main' must have a body of type BlockStatement, i.e. be defined!").getStatements
    if(roles.length == 0)
      Fail("VeyMont Fail: Main constructor is mandatory and  must assign at least one role!")
    roles.foreach {
      case a: AssignmentStatement => a.location match {
        case n: NameExpression => StructureCheck.getMainClass(source).fields().asScala.map(_.name).find(r => r == n.name) match {
          case None => Fail("VeyMont Fail: can only assign to role fields of class 'Main' in constructor")
          case Some(_) => a.expression match {
            case m: MethodInvokation => getRoleOrHelperClass(source).find(_.name == m.dispatch.getName) match {
              case None => Fail("VeyMont Fail: Wrong method: constructor of 'Main' must initialize roles with a call to a role constructor")
              case Some(_) => true
            }
            case _ => Fail("VeyMont Fail: No MethodInvokation: constructor of 'Main' must initialize roles with a call to a role constructor")
          }
        }
        case _ => Fail("VeyMont Fail: Can only assign roles, statement %s is not allowed!", a)
      }
      case _ => Fail("VeyMont Fail: constructor of 'Main' can only assign role classes")
    }
  }

  private def checkRoleNames(): Unit =
    if(getRoleNames.toSet != mainClass.fields().asScala.map(_.name).toSet) {
      Fail("VeyMont Fail: the fields of class 'Main' must be all assigned in constructor 'Main'")
    }

  private def getMainMethodsNonPureNonResourcePredicate : Iterable[Method] =
    mainClass.methods().asScala.filter(m =>
      m.name != mainMethodName &&
        m.kind != Method.Kind.Constructor &&
        m.kind != Method.Kind.Pure &&
        !isRealResource(m))

  private def checkMainMethod() : Unit = {
    val mainMethod = mainClass.methods().asScala.find(_.name == mainMethodName).get
    val b = getBlockOrThrow(mainMethod.getBody, "VeyMont Fail: expected BlockStatement for method " + mainMethodName)
    if(b.getLength != 2)
      fixedMainFail()
    else {
      checkMainBodyFirstLine(b)
      checkMainBodySecondLine(b)
    }
  }

  private def checkMainBodyFirstLine(b : BlockStatement): Unit =
    b.getStatement(0) match {
      case v : VariableDeclaration =>
        v.basetype match {
          case c : ClassType =>
            if(c.getName != mainClassName)
              fixedMainFail()
          case _ => fixedMainFail()
        }
        if(v.get().asScala.size == 1) {
          checkFirstLineConstructorCall(v,b)
        } else fixedMainFail()
      case _ => fixedMainFail()
    }

  private def checkFirstLineConstructorCall(v : VariableDeclaration, b : BlockStatement): Unit =
    v.get().asScala.head match {
      case d : DeclarationStatement =>
        d.initJava match {
          case m : MethodInvokation =>
            if(!(m.method == JavaConstructor && m.dispatch.getName == mainClassName))
              fixedMainFail()
            else b.getStatement(1) match {
              case m2 : MethodInvokation => if(d.name != m2.`object`.asInstanceOf[NameExpression].name) fixedMainFail()
              case _ => fixedMainFail()
            }
          case _ => fixedMainFail()
        }
      case _ => fixedMainFail()
    }

  private def checkMainBodySecondLine(b : BlockStatement): Unit =
    b.getStatement(1) match {
      case m : MethodInvokation =>
        if(m.method != runMethodName)
          fixedMainFail()
        else m.`object` match {
          case n : NameExpression => //fine
          case _ => fixedMainFail()
        }
      case _ => fixedMainFail()
    }

  private def checkMainMethodsAllowedSyntax(methods : Iterable[Method]) : Unit = {
    methods.foreach(m => getBlockOrThrow(m.getBody,
      "VeyMont Fail: a Plain method in class Main must have a BlockStatement body!"))
    methods.foreach(m => checkMainStatement(m.getBody))
  }

  private def checkMainStatement(s : ASTNode) : Unit = {
    s match {
      case b : BlockStatement => b.getStatements.foreach(checkMainStatement)
      case a: AssignmentStatement =>
        a.location match {
          case n : NameExpression => if(roleNames.exists(_ == n.name)) Fail("VeyMont Fail: cannot assign role anywhere else then in Main constructor")
          case _ => //fine, continue check of a.location below
        }
        getNameFromNode(a.location).map(_.name) match {
          case Some(n) => if (!roleNames.exists(_ == n)) Fail("VeyMont Fail: the assignment %s has a non-role name in its location.", a.toString)
          case None => Fail("VeyMont Fail: the assignment %s in a method of class 'Main' must have one role in its location.", a.toString)
        }
        val expNames = getNamesFromExpression(a.expression).map(_.name).filter(a => roleNames.exists(_ == a))
        if(expNames.size > 1) {
          Fail("VeyMont Fail: the assignment %s in a method of class 'Main' cannot have multiple roles in its expression.",a.toString)
        }
        val mi = getMethodInvocationsFromExpression(a.expression)
        if(mi.exists(m => m.method == Method.JavaConstructor && (m.dispatch.getName == mainClassName || roleClassNames.exists(_ == m.dispatch.getName))))
          Fail("VeyMont Fail: Cannot assign a new Main or role object in statement %s! %s", a.toString,a.getOrigin)
        if(mi.exists(_.definition.kind == Method.Kind.Pure))
          Fail("VeyMont Fail: Cannot call pure method in assignment expression! ")
        mi.foreach(mii => getNameFromNode(mii.`object`)match {
          case Some(n) => if(!roleNames.exists(_ == n.name)) Fail("VeyMont Fail: can only call role methods in assignment expression %s!", a.expression.toString)
          case None => if(mii.definition.kind != Method.Kind.Pure)
                        Fail("VeyMont Fail: cannot call non-pure methods from Main in assignment expression %s!", a.expression.toString)
        })
      case i: IfStatement => {
        if (i.getCount == 1 || i.getCount == 2) {
          if (checkSessionCondition(i.getGuard(0), roleNames)) {
            if(checkEqualRoleExpressions(prevAssertArg,i.getGuard(0))) {
              checkMainStatement(i.getStatement(0))
              if (i.getCount == 2) checkMainStatement(i.getStatement(1))
            } else Fail("VeyMont Fail: IfStatement needs to be preceded by an assert stating the equality of all the role expressions from the conditions! %s",i.getOrigin)
          } else Fail("VeyMont Fail: IfStatement needs to have one condition for each role! " + s.getOrigin)
        } else Fail("VeyMont Fail: one or two branches expected in IfStatement! " + s.getOrigin)
      }
      case l: LoopStatement => {
        if (l.getInitBlock == null && l.getUpdateBlock == null) { //it is a while loop
          if (checkSessionCondition(l.getEntryGuard, roleNames)) {
            if(checkEqualRoleExpressions(l.getContract.invariant,l.getEntryGuard))
              checkMainStatement(l.getBody)
            else Fail("VeyMont Fail: a while loop needs to have a loop invariant stating the equality of all the role expressions from the conditions! %s",l.getOrigin)
          } else Fail("VeyMont Fail: a while loop needs to have one condition for each role! " + s.getOrigin)
        } else Fail("VeyMont Fail: a for loop is not supported, use a while loop! " + s.getOrigin)
      }
      case p : ParallelRegion => {
        if (p.blocks.exists(_.block.isEmpty))
          Fail("VeyMont Fail: empty parallel block is not allowed! %s",p.getOrigin)
        if(p.blocks.exists(_.iters.nonEmpty))
          Fail("VeyMont Fail: Parallel block with iterator not allowed!")
        p.blocks.foreach(b => checkMainStatement(b.block))
      }
      case m : MethodInvokation => {
        if (m.method == mainClassName)
          Fail("This should have been detected by typechecker: cannot call method '%s'!", mainClassName)
        else if (m.method == Method.JavaConstructor && m.dispatch.getName == mainClassName)
          Fail("VeyMont Fail: cannot call constructor '%s'!", mainClassName)
        else if (m.method == Method.JavaConstructor && roleClassNames.exists(_ == m.dispatch.getName))
          Fail("VeyMont Fail: cannot call role constructor '%s'", m.dispatch.getName)
        else if (nonPlainMainMethodNames.exists(_ == m.method))
          Fail("VeyMont Fail: cannot have a method call statement for pure/predicate method '%s'! %s", m.method, m.getOrigin)
        else {
          if (mainMethodNames.exists(_ == m.method)) {
            if(m.getArity > 0)
              Fail("VeyMont Fail: methods in class Main cannot have any arguments! %s",m.getOrigin)
          } else { //it is a role or other class method
            if (m.`object` == null) {
              Fail("VeyMont Fail: method call not allowed or object of method call '%s' is not given! %s", m.method, m.getOrigin)
            }
            m.`object` match {
              case n: NameExpression =>
                if (!roleNames.exists(_ == n.name))
                  Fail("VeyMont Fail: invocation of method %s is not allowed here, because method is either pure, or from a non-role class! %s", m.method, m.getOrigin)
            }
            val roles = getNamesFromExpression(m).filter(n => roleNames.exists(_ == n.name))
            if (roles.size > 1)
              Fail("VeyMont Fail: Non-Main method call %s uses object and/or arguments from multiple roles! %s", m.toString, m.getOrigin)
          }
        }
      }
      case as : ASTSpecial =>
        if(as.kind == ASTSpecial.Kind.Assert) {
          if(as.args.length != 1) {
            Fail("VeyMont Fail: Assert can only have one argument!")
          } else prevAssertArg = as.args.head
        } else Fail("VeyMont Fail: Syntax not allowed; statement is not a session statement! " + s.getOrigin)
      case _ => Fail("VeyMont Fail: Syntax not allowed; statement is not a session statement! " + s.getOrigin)
    }
    if(!s.isInstanceOf[ASTSpecial]) //it is no ASTSpeicial, so no assert
      prevAssertArg = null
  }

  private def checkSessionCondition(node: ASTNode, roleNames : Iterable[String]) : Boolean = {
    val mi = getMethodInvocationsFromExpression(node)
    if(mi.exists(m => nonPlainMainMethodNames.exists(_ == m.method))) {
      Fail("VeyMont Fail: Cannot call pure method in if or while condition! ")
    }
    val roles = splitOnAnd(node).map(getNamesFromExpression).map(_.map(_.name).filter(a => roleNames.exists(_ == a)).toSet)
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
      case o : OperatorExpression => o.args.flatMap(getMethodInvocationsFromExpression).toSet
      case m : MethodInvokation => m.args.flatMap(getMethodInvocationsFromExpression).toSet + m
      case _ => Set.empty
    }
  }

  private def getRoleClasses : Iterable[ASTClass] = {
    val roleClassTypes = roleObjects.map(_.expression.asInstanceOf[MethodInvokation].dispatch.getName)
    getRoleOrHelperClass(source).filter(c => roleClassTypes.exists(_ == c.name))
  }

  private def checkEqualRoleExpressions(assert : ASTNode, condition : ASTNode) : Boolean = {
    var expMap : Map[ASTNode,Int] = splitOnAnd(condition).map(n => (n,0)).toMap
    for(exp <- ASTUtils.conjuncts(assert,StandardOperator.Star, StandardOperator.And).asScala) {
      exp match {
        case op : OperatorExpression => if(op.operator == StandardOperator.EQ && op.argslength == 2 && op.arg(0) != op.arg(1)) {
          op.args.foreach(a =>
            if(expMap.contains(a))
              expMap = expMap + (a -> (expMap(a) + 1))
          )
        }
        case _ => //do nothing
      }
    }
    expMap.values.sum == (expMap.size - 1) * 2
  }

  private def checkRoleMethodsTypes(source : ProgramUnit) : Unit = {
    roleClasses.foreach(_.methods().forEach(checkRoleMethodTypes(_)))
  }

  private def checkRoleMethodTypes(roleMethod : Method) : Unit = {
    if(!isNonRoleOrPrimitive(roleMethod.getReturnType,isVoid = true,allowRoles = false)) {
      Fail("VeyMont Fail: return type of method %s is a role or other unexpected type",roleMethod.name)
    }
    roleMethod.getArgs.foreach(arg => {
      if(!isNonRoleOrPrimitive(arg.`type`,isVoid = false,allowRoles = roleMethod.kind == Method.Kind.Pure)) {
        Fail("VeyMont Fail: the type of argument %s of method %s is a role or other unexpected type",arg.name,roleMethod.name)
      }
    })
  }

  private def checkRoleFieldsTypes(source : ProgramUnit) : Unit = {
    roleClasses.foreach(role => role.fields().asScala.foreach(field => {
     if(!isNonRoleOrPrimitive(field.`type`,isVoid = false,allowRoles = false))
       Fail("VeyMont Fail: type '%s' of field '%s' of role '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def isNonRoleOrPrimitive(t : Type, isVoid : Boolean, allowRoles : Boolean) : Boolean =
    isBasePrimitiveType(t, allowRoles) || isOptionOfArray(t, allowRoles) || isSequence(t, allowRoles) || isVoid && isVoidType(t)

  def isVoidType(a : ASTNode): Boolean = a match {
    case p : PrimitiveType => p.isVoid
    case _ => false
  }

  private def isBasePrimitiveType(a : ASTNode, allowRoles : Boolean) = a match {
    case p : PrimitiveType => isBaseType(p)
    case c : ClassType => isRoleOrHelperClassName(c.getName) && (allowRoles || !roleClassNames.exists(_ == c.getName))
    case _ => false
  }

  private def isBaseType(p : PrimitiveType) = StructureCheck.isAllowedPrimitive(p) || p.sort == PrimitiveSort.Resource

  private def isOptionOfArray(o : ASTNode, allowRoles : Boolean) = o match {
    case p : PrimitiveType => p.sort match {
        case PrimitiveSort.Option => p.nrOfArguments == 1 && isArray(p.args.head, allowRoles)
        case _ => false
    }
    case _ => false
  }

  @tailrec
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

  @tailrec
  private def isSequence(s : ASTNode, allowRoles : Boolean) : Boolean = s match {
    case p : PrimitiveType => p.sort match {
      case PrimitiveSort.Sequence => p.nrOfArguments == 1 && (isBasePrimitiveType(p.args.head, allowRoles) || isSequence(p.args.head, allowRoles))
      case _ => false
    }
    case _ => false
  }

  private def getOtherClasses(source : ProgramUnit) : Iterable[ASTClass] =
    source.get().asScala.collect {
      case c : ASTClass if isRoleOrHelperClassName(c.name) && !roleClassNames.exists(_ == c.name) => c
    }

  private def checkOtherClassesFieldsTypes() : Unit = {
    otherClasses.foreach(role => role.fields().asScala.foreach(field => {
      if(!isNonRoleOrPrimitive(field.`type`,isVoid = false,allowRoles = false))
        Fail("VeyMont Fail: type '%s' of field '%s' of non-role class '%s' is not allowed", field.`type`.toString, field.name, role.name)
    }))
  }

  private def checkOtherClassesMethodsTypes() : Unit = {
    otherClasses.foreach(_.methods().forEach(checkRoleMethodTypes(_)))
  }

}
