package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.{NewVariableGenerator, RewriteContractExpr}
import vct.col.ast.RewriteHelpers._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.resolve.ctx.{RefJavaParam, RefVariable}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CodeStringDefaults.predicateStore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CreatePredicates extends RewriterBuilder {
  override def key: String = "createPredicates"

  override def desc: String = "Create predicate classes into the code, so that they can be used in the assertion checking"
}


case class CreatePredicates[Pre <: Generation]() extends Rewriter[Pre] {


  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val newClasses: SuccessionMap[Declaration[_], Class[Post]] = new SuccessionMap()
  val newClassDeclarations: SuccessionMap[Variable[_], InstanceField[Post]] = new SuccessionMap()
  val newVariables: NewVariableGenerator[Pre] = new NewVariableGenerator[Pre]()

  val currentInstancePredicate: ScopedStack[InstancePredicate[Pre]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
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
      val classObjectArg: (Variable[Post], InstanceField[Post]) = createClassObjectParam()
      val instanceFields = arguments.map(generateInstanceFieldOnArg) :+ classObjectArg._2
      val predicateStore = generatePredicateStore(ip.o)
      val constructor = generateConstructorOnArgs(arguments, predicateOrigin, classObjectArg)
      instanceFields :+ predicateStore :+ constructor
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

  private def generateConstructorOnArgs(arguments: Seq[Variable[Pre]], origin: Origin, classObjectArg: (Variable[Post], InstanceField[Post])): ClassDeclaration[Post] = {
    val (newArgs, mapping) = newVariables.collect {
      arguments.map(newVariables.createNew)
    }
    val dereferences = arguments.map(createDerefOfInstanceField) :+ createDerefOfInstanceField(classObjectArg._2)
    val newJavaParams: Seq[JavaParam[Post]] = newArgs.map(generateJavaParams) :+ generateJavaParams(classObjectArg._1)
    val constructorStatements: Seq[Statement[Post]] = newJavaParams.zip(dereferences).map(a => createAssignStatement(a._1, a._2))
    val newBody = Scope[Post](Seq.empty, Block[Post](constructorStatements)(origin))(origin)
    val newJC = new JavaConstructor[Post](
      Seq(JavaPublic[Post]()(origin)),
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

  private def createClassObjectParam(): (Variable[Post], InstanceField[Post]) = {
    val cls = currentClass.top
    val classVariableOrigin = Origin(Seq()).addPrefName(cls.o.getPreferredNameOrElse().toLowerCase)
    val classVariableType = TClass[Post](newClasses.ref(cls))(classVariableOrigin)
    val newVar = new Variable[Post](classVariableType)(classVariableOrigin)
    val newInstancefield = generateInstanceFieldOnArg(cls, newVar, classVariableOrigin)
    (newVar, newInstancefield)
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

  private def generatePredicateStore(origin: Origin): CodeStringClass[Post] = {
    classDeclarations.declare(CodeStringClass[Post](predicateStore, predicateStore)(origin))
  }
}