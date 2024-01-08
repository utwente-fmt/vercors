package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.{NewVariableGenerator, NewVariableResult, RewriteContractExpr}
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{Origin, PreferredName, ShortPosition}
import vct.col.print.{Group, Text}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefJavaParam, RefVariable}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults.predicateStore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import vct.col.util.AstBuildHelpers._


object CreatePredicates extends RewriterBuilder {
  override def key: String = "createPredicates"

  override def desc: String = "Create predicate classes into the code, so that they can be used in the assertion checking"
}


case class CreatePredicates[Pre <: Generation]() extends Rewriter[Pre] {


  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val newClasses: SuccessionMap[Declaration[_], Class[Post]] = new SuccessionMap()
  val newClassDeclarations: SuccessionMap[Variable[_], InstanceField[Post]] = new SuccessionMap()
  val classInstanceField: ScopedStack[InstanceField[Post]] = new ScopedStack()
  val newVariables: NewVariableGenerator[Pre] = new NewVariableGenerator[Pre]()

  val currentInstancePredicate: ScopedStack[InstancePredicate[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = null

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test

  }


  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case ip: InstancePredicate[Pre] => currentInstancePredicate.having(ip) {
        dispatchInstancePredicate(ip)
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

  def dispatchInstancePredicate(ip: InstancePredicate[Pre]): Unit = {
    super.dispatch(ip)
    val arguments = ip.args
    val predicateOrigin = Origin(Seq.empty).addPrefName("RuntimePredicate" + ip.o.getPreferredNameOrElse().capitalize)
    val decl: Seq[ClassDeclaration[Post]] = classDeclarations.collect {
      classInstanceField.having(createClassVariableInstanceField) {
        val instanceFields = arguments.map(generateInstanceFieldOnArg) :+ classInstanceField.top
        val predicateStore = generatePredicateStore(ip)
        val constructor = generateConstructorOnArgs(arguments, predicateOrigin)
        val helperMethods = createHelperMethods(ip, arguments, instanceFields)
        instanceFields ++ helperMethods :+ predicateStore :+ constructor
      }
    }._1

    val csp = new Class[Post](decl, Seq.empty, BooleanValue(value = true)(predicateOrigin))(predicateOrigin)
    newClasses.update(ip, csp)
    globalDeclarations.declare(csp)
  }

  private def generateInstanceFieldOnArg(arg: Variable[Pre]): InstanceField[Post] = {
    val newOrigin = Origin(Seq.empty).addPrefName(arg.o.getPreferredNameOrElse())
    val res = new InstanceField(dispatch(arg.t), Set[FieldFlag[Post]]())(newOrigin)
    newClassDeclarations.update(arg, res)
    classDeclarations.declare(res)
    res
  }

  private def generateInstanceFieldOnArg(cls: Class[Pre], v: Variable[Post], origin: Origin): InstanceField[Post] = {
    val res = new InstanceField(TClass[Post](newClasses.ref(cls))(origin), Set[FieldFlag[Post]]())(origin)
    newClassDeclarations.update(v, res)
    classDeclarations.declare(res)
    res
  }

  private def generateConstructorOnArgs(arguments: Seq[Variable[Pre]], origin: Origin): ClassDeclaration[Post] = {

    val newVars = newVariables.collect {
      arguments.map(newVariables.createNew)
    }
    val dereferences = arguments.map(createDerefOfInstanceField) :+ createDerefOfInstanceField(classInstanceField.top)
    val newJavaParams: Seq[JavaParam[Post]] = newVars.outputs.map(generateJavaParams) :+ generateJavaParams(createClassVariable)
    val constructorStatements: Seq[Statement[Post]] = newJavaParams.zip(dereferences).map(a => createAssignStatement(a._1, a._2))
    val newBody = Scope[Post](Seq.empty, Block[Post](constructorStatements)(origin))(origin)
    val newJC = new JavaConstructor[Post](
      Seq(JavaPrivate[Post]()(origin)),
      origin.getPreferredNameOrElse(),
      newJavaParams,
      Seq.empty,
      Seq.empty,
      newBody,
      ApplicableContract.createEmptyContract(origin)
    )(null)(origin)
    classDeclarations.declare(newJC)
    newJC
  }

  private def createRuntimeVariable: Variable[Post] = {
    val origin = classInstanceField.top.o
    val oldName = origin.getPreferredNameOrElse().capitalize
    val newOrigin = origin.replacePrefName(s"runtime$oldName")
    val t = TClass[Post](newClasses.ref(currentInstancePredicate.top))
    new Variable[Post](t)(newOrigin)
  }

  private def createClassVariable: Variable[Post] = {
    val cls = currentClass.top
    val classVariableOrigin = Origin(Seq()).addPrefName(cls.o.getPreferredNameOrElse().toLowerCase)
    val classVariableType = TClass[Post](newClasses.ref(cls))(classVariableOrigin)
    new Variable[Post](classVariableType)(classVariableOrigin)
  }

  private def createClassVariableInstanceField: InstanceField[Post] = {
    val v = createClassVariable
    generateInstanceFieldOnArg(currentClass.top, v, v.o)
  }

  private def generateJavaParams(v: Variable[Post]): JavaParam[Post] = {
    new JavaParam[Post](Seq.empty, v.o.getPreferredNameOrElse(), v.t)(v.o)
  }

  private def createAssignStatement(arg: JavaParam[Post], deref: Deref[Post]): Statement[Post] = {
    val newLocal = JavaLocalRuntime[Post](arg.ref)(null)(arg.o)
    Assign[Post](deref, newLocal)(null)(arg.o)
  }

  private def createDerefOfInstanceField(v: Variable[Pre]): Deref[Post] = {
    val newThisObject = ThisObject[Post](newClasses.ref(currentInstancePredicate.top))(v.o)
    val instanceFieldReference: Ref[Post, InstanceField[Post]] = newClassDeclarations.ref(v)
    Deref[Post](newThisObject, instanceFieldReference)(null)(v.o)
  }

  private def createDerefOfInstanceField(instanceField: InstanceField[Post]): Deref[Post] = {
    val newThisObject = ThisObject[Post](newClasses.ref(currentInstancePredicate.top))(instanceField.o)
    val instanceFieldReference: Ref[Post, InstanceField[Post]] = instanceField.ref
    Deref[Post](newThisObject, instanceFieldReference)(null)(instanceField.o)
  }

  private def generatePredicateStore(ip: InstancePredicate[Pre]): PredicateStore[Post] = {
    val predicateType = TClass[Post](newClasses.ref(ip))
    classDeclarations.declare(PredicateStore[Post](predicateType)(ip.o))
  }

  private def createFoldMethod(): InstanceMethod[Post] = {
    //TODO should first check if the predicate holds for the input params
    //then remove the permissions for the current thread
    //after that it should create and  add the predicate to the predicate store
    ???
  }

  private def createUnfoldMethod(args: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    val o = new Origin(Seq()).addPrefName("unFold")
    val newVars = newVariables.collect {
      args.map(newVariables.createNewFromInstanceField)
    }
    val ip = currentInstancePredicate.top
    val cls: Ref[Post, Class[Post]] = newClasses.ref(ip)
    val localsFromArgs: Seq[Local[Post]] = newVars.outputs.map(v => Local[Post](v.ref)(v.o))
    val newAssertions = RewriteContractExpr[Pre](this, currentClass.top)(program, newVars)
      .createStatements(ip.body.getOrElse(BooleanValue[Pre](value = false)(Origin(Seq()))))

    //TODO remove permissions from thread (difficult first design how to do this maybe create a helperRewriter for this)

    val newRuntimePredicate = RuntimeNewPredicate[Post](createRuntimeVariable, localsFromArgs)(ip.o)
    val block = Block[Post](newAssertions._2.toSeq :+ newRuntimePredicate)(o)
    val body = Scope(Seq(), block)(o)

    val newMethod = new InstanceMethod[Post](
      TClass[Post](cls)(o),
      newVars.outputs,
      Seq(),
      Seq(),
      Some(body),
      ApplicableContract.createEmptyContract(o)
    )(null)(o)
    classDeclarations.declare(newMethod)
    newMethod


    //TODO should first check if there exists a predicate with the same paramaters in the predicatestore for this thread
    //If that is the case than it is possible to return the permissions of the thread back and remove the predicate from the predicateStore
    //if not throw error
  }

  private def createGetPredicate(args: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    val o = new Origin(Seq()).addPrefName("getPredicate")
    val newVars = newVariables.collect {
      args.map(newVariables.createNewFromInstanceField)
    }

    val localsFromArgs: Seq[Local[Post]] = newVars.outputs.map(v => Local[Post](v.ref)(v.o))
    val cls: Ref[Post, Class[Post]] = newClasses.ref(currentInstancePredicate.top)
    val body = Scope(Seq(), Block[Post](Seq(CodeStringGetPredicate(localsFromArgs, cls)(o)))(o))(o)
    val newMethod = new InstanceMethod[Post](
      TClass[Post](cls)(o),
      newVars.outputs,
      Seq(),
      Seq(),
      Some(body),
      ApplicableContract.createEmptyContract(o)
    )(null)(o)
    classDeclarations.declare(newMethod)
    newMethod
  }

  private def createEqualsMethod(instanceFields: Seq[InstanceField[Post]]): InstanceMethod[Post] = {
    val args = Seq(createRuntimeVariable, createRuntimeVariable)
    val o = new Origin(Seq()).addPrefName("equals")
    val bodyStatements = createEqualsStatements(args(0), args(1), instanceFields) :+ Return[Post](BooleanValue(value = true)(o))(o)
    val body = Scope(Seq(), Block[Post](bodyStatements)(o))(o)
    val newMethod = new InstanceMethod[Post](
      TBool[Post](),
      args,
      Seq(),
      Seq(),
      Some(body),
      ApplicableContract.createEmptyContract(o)
    )(null)(o)
    classDeclarations.declare(newMethod)
    newMethod
  }


  private def createEqualsStatements(currentObject: Variable[Post], arg: Variable[Post], instanceFields: Seq[InstanceField[Post]]): Seq[Statement[Post]] = {
    instanceFields.map(i => createEqualsStatement(currentObject, arg, i))
  }

  private def createEqualsStatement(currentObject: Variable[Post], arg: Variable[Post], instanceField: InstanceField[Post]): Branch[Post] = {
    implicit val origin: Origin = Origin(Seq(ShortPosition("Generated"), PreferredName("equals")))
    val obj = createEqualsDeref(currentObject, instanceField)
    val target = createEqualsDeref(arg, instanceField)
    val comparison = instanceField.t match {
      case _ : TClass[Post] => !(obj ==== target) //if it is an object use the equals method otherwise use the != operator
      case _ => obj !== target
    }
    val returnStatement = Return[Post](tt)
    Branch[Post](Seq((comparison, returnStatement)))(obj.o)
  }

  private def createEqualsDeref(v: Variable[Post], instanceField: InstanceField[Post]): Deref[Post] = {
    val local = Local[Post](v.ref)(v.o)
    Deref[Post](local, instanceField.ref)(null)(v.o)
  }

  private def createHelperMethods(ip: InstancePredicate[Pre], args: Seq[Variable[Pre]], instanceFields: Seq[InstanceField[Post]]): Seq[InstanceMethod[Post]] = {
    val equalsMethod = createEqualsMethod(instanceFields)
    val getPredicate = createGetPredicate(instanceFields)
//    val unFold = createUnfoldMethod(instanceFields)
    //TODO fix fold and unfold with the newly created locals
    Seq(equalsMethod, getPredicate)
  }


}