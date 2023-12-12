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

  val newClasses: SuccessionMap[InstancePredicate[Pre], Class[Post]] = new SuccessionMap()
  val newClassDeclarations: SuccessionMap[Variable[Pre], InstanceField[Post]] = new SuccessionMap()
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
      case _ => super.dispatch(decl)
    }
  }

  def dispatchInstancePredicate(ip: InstancePredicate[Pre]): Unit = {
    super.dispatch(ip)
    val arguments = ip.args
    val predicateOrigin = Origin(Seq.empty).addPrefName("RuntimePredicate" + ip.o.getPreferredNameOrElse().capitalize)
    val decl: Seq[ClassDeclaration[Post]] = classDeclarations.collect {
      val instanceFields = arguments.map(generateInstanceFieldOnArg)
      val predicateStore = generatePredicateStore(ip.o)
      val constructor = generateConstructorOnArgs(arguments, predicateOrigin)
      instanceFields :+ predicateStore :+ constructor
    }._1

    val csp = new Class[Post](decl, Seq.empty, BooleanValue(true)(predicateOrigin))(predicateOrigin)
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

  private def generateConstructorOnArgs(arguments: Seq[Variable[Pre]], origin: Origin): ClassDeclaration[Post] = {
    val (newArgs, mapping) = newVariables.collect {
      arguments.map(newVariables.createNew)
    }
    val dereferences = arguments.map(createDerefOfInstanceField)
    val newJavaParams = newArgs.map(generateJavaParams)
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

  private def generatePredicateStore(origin: Origin): CodeStringClass[Post] = {
    classDeclarations.declare(CodeStringClass[Post](predicateStore, predicateStore)(origin))
  }
}