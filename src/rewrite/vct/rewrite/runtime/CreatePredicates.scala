package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{Origin, PreferredName, ShortPosition}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.RewriteContractExpr


object CreatePredicates extends RewriterBuilder {
  override def key: String = "createPredicates"

  override def desc: String = "Create predicate classes into the code, so that they can be used in the assertion checking"
}


case class CreatePredicates[Pre <: Generation]() extends Rewriter[Pre] {


  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val newClasses: SuccessionMap[Declaration[_], Class[Post]] = new SuccessionMap()
  val newClassDeclarations: SuccessionMap[Variable[_], InstanceField[Post]] = new SuccessionMap()
  val classInstanceField: ScopedStack[InstanceField[Post]] = new ScopedStack()
  val currentIP: ScopedStack[InstancePredicate[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = _

  /**
   * Dispatch of the program for debugging and using the program everywhere to look up specific instancefields
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
   * @param ip The InstancePredicate that needs to be transformed to a class
   */
  def dispatchIP(ip: InstancePredicate[Pre]): Unit = {
    implicit val origin: Origin = Origin(Seq.empty).addPrefName("RuntimePredicate" + ip.o.getPreferredNameOrElse().capitalize)
    super.dispatch(ip)
    val arguments = ip.args
    val decl: Seq[ClassDeclaration[Post]] = classDeclarations.collect {
      classInstanceField.having(createClassVariableInstanceField) {
        val instanceFields = arguments.map(generateInstanceFieldOnArg) :+ classInstanceField.top
        val predicateStore = generatePredicateStore(ip)
        val constructor = generateConstructorOnArgs(arguments)
        val helperMethods = createHelperMethods(ip, arguments, instanceFields)
        Seq(constructor, predicateStore) ++ instanceFields ++ helperMethods
      }
    }._1

    val csp = new Class[Post](decl, Seq.empty, BooleanValue(value = true))
    newClasses.update(ip, csp)
    globalDeclarations.declare(csp)
  }

  /**
   * Creates an InstanceField based on paramaters of the predicate
   * @param arg One paramater of the predicate
   * @return InstanceField for the created parameter
   */
  private def generateInstanceFieldOnArg(arg: Variable[Pre]): InstanceField[Post] = {
    val newOrigin = Origin(Seq.empty).addPrefName(arg.o.getPreferredNameOrElse())
    val res = new InstanceField(dispatch(arg.t), Set[FieldFlag[Post]]())(newOrigin)
    newClassDeclarations.update(arg, res)
    classDeclarations.declare(res)
    res
  }

  /**
   * Creates constructor for the Predicate class
   * @param arguments all paramaters of the constructor of the predicate class
   * @param origin implicit origin of the new class
   * @return returns the newly created constructor for the predicate class
   */
  private def generateConstructorOnArgs(arguments: Seq[Variable[Pre]])(implicit origin: Origin): ClassDeclaration[Post] = {

    val (outputs, result) = variables.collect {
      arguments.map(a => variables.succeed(a, a.rewrite()))
    }
    val dereferences = arguments.map(createDerefOfInstanceField) :+ createDerefOfInstanceField(classInstanceField.top)
    val newJavaParams: Seq[JavaParam[Post]] = outputs.map(generateJavaParams) :+ generateJavaParams(createClassVariable)
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
   * @param v Input variable
   * @return variable that is transformed to a JavaParam
   */
  private def generateJavaParams(v: Variable[Post]): JavaParam[Post] = {
    new JavaParam[Post](Seq.empty, v.o.getPreferredNameOrElse(), v.t)(v.o)
  }

  /**
   * Creates an assign statement using a java param and a dereference to an instancefied
   * @param arg
   * @param deref
   * @return
   */
  private def createAssignStatement(arg: JavaParam[Post], deref: Deref[Post]): Statement[Post] = {
    val newLocal = JavaLocalRuntime[Post](arg.ref)(null)(arg.o)
    Assign[Post](deref, newLocal)(null)(arg.o)
  }

  /**
   * Creates a dereference of an instanceField using a variable
   * @param v
   * @return
   */
  private def createDerefOfInstanceField(v: Variable[Pre]): Deref[Post] = {
    val newThisObject = ThisObject[Post](newClasses.ref(currentIP.top))(v.o)
    val instanceFieldReference: Ref[Post, InstanceField[Post]] = newClassDeclarations.ref(v)
    Deref[Post](newThisObject, instanceFieldReference)(null)(v.o)
  }

  /**
   * creates a dereference of an instance field using an instanceField
   * @param instanceField
   * @return
   */
  private def createDerefOfInstanceField(instanceField: InstanceField[Post]): Deref[Post] = {
    val newThisObject = ThisObject[Post](newClasses.ref(currentIP.top))(instanceField.o)
    val instanceFieldReference: Ref[Post, InstanceField[Post]] = instanceField.ref
    Deref[Post](newThisObject, instanceFieldReference)(null)(instanceField.o)
  }

  /**
   * Creates a new instancefield for the class that the predicate was build in
   * @return Instancefield of the currentclass
   */
  private def createClassVariableInstanceField: InstanceField[Post] = {
    val v = createClassVariable
    generateInstanceFieldOnArg(currentClass.top, v, v.o)
  }

  /**
   * Based on the cls, a variable and origin create an instanceField. This is used for creating instancefields for arguments
   * @param cls
   * @param v
   * @param origin
   * @return
   */
  private def generateInstanceFieldOnArg(cls: Class[Pre], v: Variable[Post], origin: Origin): InstanceField[Post] = {
    val res = new InstanceField(TClass[Post](newClasses.ref(cls))(origin), Set[FieldFlag[Post]]())(origin)
    newClassDeclarations.update(v, res)
    classDeclarations.declare(res)
    res
  }

  /**
   * Creates a variable of the class that holds the predicate
   * @return
   */
  private def createClassVariable: Variable[Post] = {
    val cls = currentClass.top
    val classVariableOrigin = Origin(Seq()).addPrefName(cls.o.getPreferredNameOrElse().toLowerCase)
    val classVariableType = TClass[Post](newClasses.ref(cls))(classVariableOrigin)
    new Variable[Post](classVariableType)(classVariableOrigin)
  }

  /**
   * Creates the predicateStore in the predicate class
   * @param ip
   * @return
   */
  private def generatePredicateStore(ip: InstancePredicate[Pre]): PredicateStore[Post] = {
    val predicateType = TClass[Post](newClasses.ref(ip))
    classDeclarations.declare(PredicateStore[Post](predicateType)(ip.o))
  }

  /**
   * Creates the helper methods for the predicate class
   * @param ip
   * @param args
   * @param instanceFields
   * @return
   */
  private def createHelperMethods(ip: InstancePredicate[Pre], args: Seq[Variable[Pre]], instanceFields: Seq[InstanceField[Post]]): Seq[InstanceMethod[Post]] = {
    val equalsMethod = createEqualsMethod(instanceFields)
    val getPredicate = createGetPredicateMethod(instanceFields)
    val unFold = createUnfoldMethod(instanceFields)
//    val fold = createFoldMethod(instanceFields)
    //TODO fix fold and unfold with the newly created locals
    Seq(equalsMethod, getPredicate, unFold)
  }

  /**
   * Creates the equals method
   * @param instanceFields
   * @return
   */
  private def createEqualsMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("equals")
    createMethod(instanceFields, createMethodBodyEquals)
  }

  /**
   * Creates the getPredicate method
   * @param instanceFields
   * @return
   */
  private def createGetPredicateMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("getPredicate")
    createMethod(instanceFields, createMethodBodyGetPredicate)
  }

  /**
   * Creates the Unfold Method
   * @param instanceFields
   * @return
   */
  private def createUnfoldMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("unfold")
    createMethod(instanceFields, createMethodBodyUnfold)
  }

  /**
   * Creates the fold method
   * @param instanceFields
   * @return
   */
  private def createFoldMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    implicit val origin: Origin = new Origin(Seq()).addPrefName("fold")
    createMethod(instanceFields, createMethodBodyFold)
  }

  /**
   * Case class to easily have all the inputs to generate a method body
   * @param instanceFields
   * @param newArgs
   * @param ip
   * @param clsRef
   * @param locals
   */
  private case class MethodBodyInputs(instanceFields: Seq[InstanceField[Post]],
                                      newArgs: Seq[Variable[Post]],
                                      ip : InstancePredicate[Pre],
                                      clsRef: Ref[Post, Class[Post]],
                                      locals: Seq[Local[Post]]) {
    /**
     * Creates a MethodBodyResult based on own inputs and the newly created statement that the body of the method is
     * @param statement
     * @return
     */
    def unit(statement: Statement[Post]): MethodBodyResult = MethodBodyResult(newArgs, TClass[Post](clsRef), statement)
  }

  /**
   * MethodBodyResult is a case class to easily create instanceMethod based on the returned arguments
   * @param newArgs
   * @param returnType
   * @param statement
   */
  private case class MethodBodyResult(newArgs: Seq[Variable[Post]],
                                      returnType: Type[Post],
                                      statement: Statement[Post]){
    def getInstanceMethod(implicit o: Origin): InstanceMethod[Post] = {
      val body = Scope(Seq(), statement)
      val newMethod = new InstanceMethod[Post](
        returnType,
        newArgs,
        Seq(),
        Seq(),
        Some(body),
        ApplicableContract.createEmptyContract
      )(null)
      classDeclarations.declare(newMethod)
    }
  }

  /**
   * Default template to create a instanceMethod
   * @param instanceFields all instancefields of the predicate class
   * @param f function that specifically transforms a methodbodyinput to a methodbodyresult
   * @param origin
   * @return
   */
  private def createMethod(instanceFields: Seq[InstanceField[Post]], f: MethodBodyInputs => MethodBodyResult)(implicit origin: Origin): InstanceMethod[Post] = {
    variables.collectScoped {
      val newArgs: Seq[Variable[Post]] = instanceFields.map(a => new Variable[Post](a.t))
      val ip: InstancePredicate[Pre] = currentIP.top
      val clsRef: Ref[Post, Class[Post]] = newClasses.ref(ip)
      val locals: Seq[Local[Post]] = newArgs.map(v => Local[Post](v.ref)(v.o))
      val mbi: MethodBodyInputs = MethodBodyInputs(instanceFields, newArgs, ip, clsRef, locals)
      val mbr: MethodBodyResult = f(mbi)
      mbr.getInstanceMethod
    }._2
  }

  /**
   * Creates the body of the equals method
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyEquals(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    val (first, second) = (createRuntimeVariable, createRuntimeVariable)
    val instanceFieldsStatements: Seq[Statement[Post]] = mbi.instanceFields.map(i => createEqualsStatement(first, second, i))
    val bodyStatements = instanceFieldsStatements :+ Return[Post](tt)
    MethodBodyResult(Seq(first, second), TBool[Post](), Block[Post](bodyStatements))
  }

  /**
   * Creates a statement to check if a paramater of the classes are the same, if not return false
   * @param currentObject
   * @param arg
   * @param instanceField
   * @return
   */
  private def createEqualsStatement(currentObject: Variable[Post], arg: Variable[Post], instanceField: InstanceField[Post]): Branch[Post] = {
    implicit val origin: Origin = Origin(Seq(ShortPosition("Generated"), PreferredName("equals")))
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
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyGetPredicate(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    mbi.unit(Block[Post](Seq(CodeStringGetPredicate(mbi.locals, mbi.clsRef))))
  }

  /**
   * Creates the body for the unfold method
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyUnfold(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    val newAssertions = RewriteContractExpr[Pre](this, currentClass.top)(program)
      .createStatements(mbi.ip.body.getOrElse(ff))
    val newRuntimePredicate = RuntimeNewPredicate[Post](createRuntimeVariable, mbi.locals)(mbi.ip.o)
    mbi.unit(Block[Post](newAssertions._2.toSeq :+ newRuntimePredicate))
    //TODO remove permissions from thread (difficult first design how to do this maybe create a helperRewriter for this)
    //TODO should first check if there exists a predicate with the same paramaters in the predicatestore for this thread
  }

  /**
   * creates the body for the fold method
   * @param mbi
   * @param origin
   * @return
   */
  private def createMethodBodyFold(mbi: MethodBodyInputs)(implicit origin: Origin): MethodBodyResult = {
    mbi.unit(Block[Post](Seq(Return[Post](tt))))
  }

  /**
   * Creates a variables of the newly created predicate class
   * @return
   */
  private def createRuntimeVariable: Variable[Post] = {
    val origin = classInstanceField.top.o
    val oldName = origin.getPreferredNameOrElse().capitalize
    val newOrigin = origin.replacePrefName(s"runtime$oldName")
    val t = TClass[Post](newClasses.ref(currentIP.top))
    variables.declare(new Variable[Post](t)(newOrigin))
  }
}