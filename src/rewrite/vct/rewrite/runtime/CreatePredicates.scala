package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


object CreatePredicates extends RewriterBuilder {
  override def key: String = "createPredicates"

  override def desc: String = "Create predicate classes into the code, so that they can be used in the assertion checking"
}


case class CreatePredicates[Pre <: Generation]() extends Rewriter[Pre] {


  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val newClasses: SuccessionMap[Declaration[_], Class[Post]] = new SuccessionMap()
  val newClassDeclarations: SuccessionMap[Declaration[_], InstanceField[Post]] = new SuccessionMap()
  val currentIP: ScopedStack[InstancePredicate[Pre]] = new ScopedStack()
  val helperMethods: ScopedStack[ArrayBuffer[InstanceMethod[Post]]] = new ScopedStack()

  implicit var program: Program[Pre] = _

  /**
   * Dispatch of the program for debugging and using the program everywhere to look up specific instancefields
   *
   * @param program Program node
   * @return The rewritten program
   */
  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }

  /**
   * Dispatches declarations
   * InstancePredicates will be rewritten to new classes
   * Class will be stored so it can be later used in the code
   *
   * @param decl Any declaration kind
   */
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case ip: InstancePredicate[Pre] => currentIP.having(ip) {
        dispatchIP(ip)
      }
      case cls: Class[Pre] => {
        currentClass.having(cls) {
          val newClass = RewriteClass(cls)(this).rewrite()
          newClasses.update(cls, newClass)
          globalDeclarations.succeed(cls, newClass)
        }
      }
      case _ => super.dispatch(decl)
    }
  }

  /**
   * Method that specifically dispatches an InstancePredicate
   * First it will create a new class, with instancefields, a predicate store, a constructor and helper methods
   * such as getPredicate, (un)fold and equals
   *
   * @param ip The InstancePredicate that needs to be transformed to a class
   */
  def dispatchIP(ip: InstancePredicate[Pre]): Unit = {
    implicit val origin: Origin = Origin(Seq.empty).addPrefName("RuntimePredicate" + ip.o.getPreferredNameOrElse().capitalize)
    super.dispatch(ip)
    val arguments: Seq[Variable[Pre]] = ip.args
    val decl: Seq[ClassDeclaration[Post]] = classDeclarations.collect {
        val instanceFields = (arguments :+ currentClass.top).map(generateInstanceFieldOnArg)
        val predicateStore = classDeclarations.declare(PredicateStore[Post](TClass[Post](newClasses.ref(ip)))(ip.o))
        val constructor = generateConstructorOnArgs(arguments)
        val helperMethods = createHelperMethods(ip, arguments, instanceFields)
        Seq(constructor, predicateStore) ++ instanceFields ++ helperMethods
    }._1

    val csp = new Class[Post](decl, Seq.empty, BooleanValue(value = true))
    newClasses.update(ip, csp)
    globalDeclarations.declare(csp)
  }

  private def generateInstanceFieldOnArg(decl: Declaration[Pre]) : InstanceField[Post] = {
    implicit val origin = Origin(Seq.empty).addPrefName(decl.o.getPreferredNameOrElse())
    val t: Type[Rewritten[Pre]] = decl match {
      case cls: Class[Pre] => TClass[Post](newClasses.ref(cls))
      case v: Variable[Pre] => dispatch(v.t)
      case _ => throw Unreachable("Only Class[Pre] and Variable[Pre] are allowed in this method")
    }
    val res = new InstanceField[Post](t, Set[FieldFlag[Post]]())
    newClassDeclarations.update(decl, res)
    classDeclarations.declare(res)
  }


  /**
   * Creates constructor for the Predicate class
   *
   * @param arguments all paramaters of the constructor of the predicate class
   * @param origin    implicit origin of the new class
   * @return returns the newly created constructor for the predicate class
   */
  private def generateConstructorOnArgs(arguments: Seq[Variable[Pre]])(implicit origin: Origin): ClassDeclaration[Post] = {
    val (_, outputs) = variables.collectScoped{
      createArgumentsVariables
    }
    val dereferences = (arguments :+ currentClass.top).map(createDerefOfInstanceField)
    val newJavaParams: Seq[JavaParam[Post]] = outputs.map(generateJavaParams)
    val constructorStatements: Seq[Statement[Post]] = newJavaParams.zip(dereferences).map(a => createAssignStatement(a._1, a._2))
    val newBody = Scope[Post](Seq.empty, Block[Post](constructorStatements))
    val newJC = new JavaConstructor[Post](
      Seq(JavaPrivate[Post]()(origin)),
      origin.getPreferredNameOrElse(),
      newJavaParams,
      Seq.empty,
      Seq.empty,
      newBody,
      ApplicableContract.createEmptyContract
    )(null)
    classDeclarations.declare(newJC)
    newJC
  }

  /**
   * Transforms a Variable to a JavaParam
   *
   * @param v Input variable
   * @return variable that is transformed to a JavaParam
   */
  private def generateJavaParams(v: Variable[Post]): JavaParam[Post] = {
    new JavaParam[Post](Seq.empty, v.o.getPreferredNameOrElse(), v.t)(v.o)
  }

  /**
   * Creates an assign statement using a java param and a dereference to an instancefied
   *
   * @param arg
   * @param deref
   * @return
   */
  private def createAssignStatement(arg: JavaParam[Post], deref: Deref[Post]): Statement[Post] = {
    val newLocal = JavaLocalRuntime[Post](arg.ref)(null)(arg.o)
    Assign[Post](deref, newLocal)(null)(arg.o)
  }

  private def createDerefOfInstanceField(decl: Declaration[Pre]): Deref[Post] = {
    val newThisObject = ThisObject[Post](newClasses.ref(currentIP.top))(decl.o)
    val instanceFieldReference: Ref[Post, InstanceField[Post]] = newClassDeclarations.ref(decl)
    Deref[Post](newThisObject, instanceFieldReference)(null)(decl.o)
  }

  /**
   * Creates a variables of the newly created predicate class
   *
   * @return
   */
  private def createRuntimeVariable: Variable[Post] = {
    val origin = Origin(Seq.empty)
    val oldName = currentClass.top.o.getPreferredNameOrElse().capitalize
    val newOrigin = origin.replacePrefName(s"runtime$oldName")
    val t = TClass[Post](newClasses.ref(currentIP.top))
    variables.declare(new Variable[Post](t)(newOrigin))
  }

  private def createArgumentsVariables : Seq[Variable[Post]] = {
    val cls = currentClass.top
    val classVariableOrigin = Origin(Seq()).addPrefName(cls.o.getPreferredNameOrElse().toLowerCase)
    val classVariableType = TClass[Post](newClasses.ref(cls))(classVariableOrigin)
    val clsVar = variables.declare(new Variable[Post](classVariableType)(classVariableOrigin))
    currentIP.top.args.map(a => variables.succeed(a, a.rewrite())) :+ clsVar
  }



  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                                        Create methods for the predicate                                          //
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




  /**
   * Creates the helper methods for the predicate class
   *
   * @param ip
   * @param args
   * @param instanceFields
   * @return
   */
  private def createHelperMethods(ip: InstancePredicate[Pre], args: Seq[Variable[Pre]], instanceFields: Seq[InstanceField[Post]]): Seq[InstanceMethod[Post]] = {
    helperMethods.collect{
      val equalsMethod = createEqualsMethod(instanceFields)
      val getPredicate = createGetPredicateMethod(instanceFields)
      val unfold = createUnfoldMethod(instanceFields)
      val fold = createFoldMethod(instanceFields)
      Seq(equalsMethod, getPredicate, unfold, fold)
    }._2
  }

  /**
   * Creates the equals method
   *
   * @param instanceFields
   * @return
   */
  private def createEqualsMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("equals")
    createMethod(instanceFields, createMethodBodyEquals)
  }

  /**
   * Creates the getPredicate method
   *
   * @param instanceFields
   * @return
   */
  private def createGetPredicateMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("getPredicate")
    createMethod(instanceFields, createMethodBodyGetPredicate)
  }

  /**
   * Creates the Unfold Method
   *
   * @param instanceFields
   * @return
   */
  private def createUnfoldMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("unfold")
    createMethod(instanceFields, createMethodBodyUnfold)
  }

  /**
   * Creates the fold method
   *
   * @param instanceFields
   * @return
   */
  private def createFoldMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("fold")
    createMethod(instanceFields, createMethodBodyFold)
  }

  /**
   * Case class to easily have all the inputs to generate a method body
   *
   * @param instanceFields
   * @param newArgs
   * @param ip
   * @param clsRef
   * @param argsLocals
   */
  private case class MethodBodyInputs(instanceFields: Seq[InstanceField[Post]],
                                      newArgs: Seq[Variable[Post]],
                                      ip: InstancePredicate[Pre],
                                      clsRef: Ref[Post, Class[Post]],
                                      argsLocals: Seq[Local[Post]]) {
    /**
     * Creates a MethodBodyResult based on own inputs and the newly created statement that the body of the method is
     *
     * @param statement
     * @return
     */
    def unit(statement: Statement[Post]): MethodBodyResult = MethodBodyResult(newArgs, TClass[Post](clsRef), statement)
  }

  /**
   * MethodBodyResult is a case class to easily create instanceMethod based on the returned arguments
   *
   * @param newArgs
   * @param returnType
   * @param statement
   */
  private case class MethodBodyResult(newArgs: Seq[Variable[Post]],
                                      returnType: Type[Post],
                                      statement: Statement[Post]) {
    def getInstanceMethod(locals: Seq[Variable[Post]])(implicit o: Origin): InstanceMethod[Post] = {
      val body = Scope(locals, statement)
      val newMethod = new InstanceMethod[Post](
        returnType,
        newArgs,
        Seq(),
        Seq(),
        Some(body),
        ApplicableContract.createEmptyContract
      )(null)
      helperMethods.top.addOne(newMethod)
      classDeclarations.declare(newMethod)
    }
  }

  /**
   * Default template to create a instanceMethod
   *
   * @param instanceFields all instancefields of the predicate class
   * @param f              function that specifically transforms a methodbodyinput to a methodbodyresult
   * @param origin
   * @return
   */
  private def createMethod(instanceFields: Seq[InstanceField[Post]], f: MethodBodyInputs => MethodBodyResult)(implicit origin: Origin): InstanceMethod[Post] = {
    variables.collectScoped {
      val ip: InstancePredicate[Pre] = currentIP.top
      val newArgs: Seq[Variable[Post]] = createArgumentsVariables
      val clsRef: Ref[Post, Class[Post]] = newClasses.ref(ip)
      val argsLocals: Seq[Local[Post]] = newArgs.map(v => Local[Post](v.ref)(v.o))
      val mbi: MethodBodyInputs = MethodBodyInputs(instanceFields, newArgs, ip, clsRef, argsLocals)
      val (newLocals: Seq[Variable[Post]], mbr: MethodBodyResult) = variables.collect {
        f(mbi)
      }
      mbr.getInstanceMethod(newLocals)
    }._2
  }

  /**
   * Creates the body of the equals method
   *
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyEquals(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {

    val (first, second) = variables.collect{(createRuntimeVariable, createRuntimeVariable)}._2
    val instanceFieldsStatements: Seq[Statement[Post]] = mbi.instanceFields.map(i => createEqualsStatement(first, second, i))
    val bodyStatements = instanceFieldsStatements :+ Return[Post](tt)
    MethodBodyResult(Seq(first, second), TBool[Post](), Block[Post](bodyStatements))
  }

  /**
   * Creates a statement to check if a paramater of the classes are the same, if not return false
   *
   * @param currentObject
   * @param arg
   * @param instanceField
   * @return
   */
  private def createEqualsStatement(currentObject: Variable[Post], arg: Variable[Post], instanceField: InstanceField[Post])(implicit origin: Origin): Branch[Post] = {
    val obj = createEqualsDeref(currentObject, instanceField)
    val target = createEqualsDeref(arg, instanceField)
    val comparison = instanceField.t match {
      case _: TClass[Post] => !(obj ==== target) //if it is an object use the equals method otherwise use the != operator
      case _ => obj !== target
    }
    val returnStatement = Return[Post](ff)
    Branch[Post](Seq((comparison, returnStatement)))(obj.o)

  }

  /**
   * Creates a dereference for a variable to its instancefield
   *
   * @param v
   * @param instanceField
   * @return
   */
  private def createEqualsDeref(v: Variable[Post], instanceField: InstanceField[Post]): Deref[Post] = {
    val local = Local[Post](v.ref)(v.o)
    Deref[Post](local, instanceField.ref)(null)(v.o)
  }

  /**
   * Creates the method block of the getpredicate method
   *
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyGetPredicate(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    val tmpPredicate = variables.declare(new Variable[Post](TClass[Post](mbi.clsRef))(Origin(Seq()).addPrefName("tmp")))
    val newPredicate = RuntimeNewPredicate[Post](mbi.clsRef, mbi.argsLocals)
    val assignTmp = Eval[Post](PostAssignExpression[Post](tmpPredicate.get, newPredicate)(null))
    val loopVariable = variables.collectScoped {variables.declare(new Variable[Post](TClass[Post](mbi.clsRef))(Origin(Seq()).addPrefName("p")))}._2
    val loopBody = Block[Post](Seq(Branch[Post](Seq((Equals[Post](tmpPredicate.get, loopVariable.get), Return[Post](loopVariable.get))))))
    val enhancedFor = EnhancedLoop[Post](loopVariable, PredicateStoreGet[Post](mbi.clsRef, ThreadId[Post](None)), loopBody)
    val postReturnNull = Return[Post](Null[Post]())
    val methodBody = Block[Post](Seq(assignTmp, enhancedFor, postReturnNull))
    mbi.unit(methodBody)
  }

  /**
   * Creates the body for the unfold method
   *
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyUnfold(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    val tmpPredicate = variables.declare(new Variable[Post](TClass[Post](mbi.clsRef))(Origin(Seq()).addPrefName("predicate")))
    val getPredicateInstanceMethod: InstanceMethod[Post] = helperMethods.top.find(p => p.o.getPreferredNameOrElse() == "getPredicate").get
    val getPredicateCall = MethodInvocation[Post](ThisObject[Post](mbi.clsRef), getPredicateInstanceMethod.ref, mbi.argsLocals, Seq.empty, Seq.empty, Seq.empty, Seq.empty)(null)
    val assignTmp = Eval[Post](PostAssignExpression[Post](tmpPredicate.get, getPredicateCall)(null))
    val nullCheck = Assert[Post](tmpPredicate.get !== Null[Post]())(null)
    //We know that tmpPredicate is now not null
    val getThreadSpecificPredicateStore = PredicateStoreGet[Post](mbi.clsRef, ThreadId[Post](None))
    val removePredicate = Assert[Post](CopyOnWriteArrayListRemove[Post](getThreadSpecificPredicateStore, tmpPredicate.get))(null) //Inside an assert, because it should remove it, if it does not remove it something went wrong
    val pd: PermissionData[Pre] = PermissionData()
      .setOuter(this)
      .setCls(currentClass.top)
      .setOffset(mbi.argsLocals.last)
    val addPermissions: Block[Post] = TransferPermissionRewriter(pd).addPermissions(mbi.ip.body.getOrElse(tt[Pre]))
    val methodBody = Block[Post](Seq(assignTmp, nullCheck, removePredicate, addPermissions))
    mbi.unit(methodBody)
  }

  /**
   * creates the body for the fold method
   *
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyFold(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    val assertPD: PermissionData[Pre] = PermissionData().setOuter(this).setCls(currentClass.top).setOffset(mbi.argsLocals.last)
    val assertion: Block[Post] = RewriteContractExpr[Pre](assertPD).createAssertions(mbi.ip.body.getOrElse(tt))
    val removePD: PermissionData[Pre] = PermissionData().setOuter(this).setCls(currentClass.top).setOffset(mbi.argsLocals.last)
    val removePermissions: Block[Post] = TransferPermissionRewriter(removePD).removePermissions(mbi.ip.body.getOrElse(tt))
    val newRuntimePredicate: RuntimeNewPredicate[Post] = RuntimeNewPredicate[Post](mbi.clsRef, mbi.argsLocals)
    val getThreadSpecificPredicateStore = PredicateStoreGet[Post](mbi.clsRef, ThreadId[Post](None))
    val addPredicateToStore : Eval[Post] = Eval(CopyOnWriteArrayListAdd(getThreadSpecificPredicateStore, newRuntimePredicate))
    val methodBody: Block[Post] = Block(Seq(assertion, removePermissions, addPredicateToStore))
    mbi.unit(methodBody)
  }
}