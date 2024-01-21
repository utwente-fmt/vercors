package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteVariable
import vct.col.ast.{And, Expr, Variable, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.{FrozenScopes, SuccessionMap}
import vct.col.util.AstBuildHelpers._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


abstract class AbstractQuantifierRewriter[Pre <: Generation](val outer: Rewriter[Pre], val cls: Class[Pre], val extraArgs: Seq[Variable[Pre]] = Seq.empty)(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def dispatchLoopBody(quantifier: Expr[Pre], left: Expr[Pre], right: Expr[Pre], newArgs: Seq[Variable[Pre]]): Seq[Statement[Post]] = Seq()

  def dispatchPostBody(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], quantifierId: String, newLocals: Seq[Variable[Post]]): Seq[Statement[Post]] = Seq()


  final def defineLoopCondition(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    val loopCondition = (Not[Post](dispatch(condition))(expr.o), Continue[Post](None)(expr.o))
    Branch[Post](Seq(loopCondition))(expr.o)
  }


  final def createBody(expr: Expr[Pre], left: Expr[Pre], right: Expr[Pre], newArgs: Seq[Variable[Pre]]): Statement[Post] = {
    val loopConditionBranch = defineLoopCondition(expr, left)
    val test = variables.freeze
    val dispatchedLoopBody = dispatchLoopBody(expr, left, right, newArgs)
    Block[Post](Seq(loopConditionBranch) ++ dispatchedLoopBody)(expr.o)
  }

  final def createQuantifier(expr: Expr[Pre], acc: Statement[Post], element: Variable[Pre], filteredBounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])]): CodeStringQuantifier[Post] = {
    implicit val origin: Origin = expr.o
    CodeStringQuantifier[Post](
      Local[Post](variables.freeze.succ(element)),
      filteredBounds.map(i => i._2).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MinValue)),
      filteredBounds.map(i => i._3).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MaxValue)),
      acc
    )(expr.o)
  }

  final def createBodyQuantifier(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], newArgs: Seq[Variable[Pre]]): Statement[Post] = {
    val bounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])] = FindBoundsQuantifier[Pre](this).findBounds(expr)
    val loopBody = createBody(expr, left, right, newArgs)
    bindings.reverse.foldLeft[Statement[Post]](loopBody)((acc, element) =>
      createQuantifier(expr, acc, element, bounds.filter(i => i._1 == element))
    )
  }

  final def declareNewMethod(expr: Expr[Pre], quantifierId: String, arguments: Seq[Variable[Post]], newLocals: Seq[Variable[Post]], methodBlock: Block[Post]): InstanceMethod[Post] = {
    implicit val o: Origin = expr.o.replacePrefName("__runtime_quantifier__" + quantifierId)
    val newMethod = new InstanceMethod[Post](
      TBool(),
      arguments,
      Seq.empty,
      Seq.empty,
      Some(Scope[Post](newLocals, methodBlock)(expr.o)),
      ApplicableContract.createEmptyContract
    )(null)
    classDeclarations.declare(newMethod)
    newMethod
  }

  final def createMethodCall(expr: Expr[Pre], quantifierId: String, newMethod: InstanceMethod[Post], args: Seq[Expr[Post]]): MethodInvocation[Post] = {
    implicit val origin: Origin = expr.o.replacePrefName("__runtime_quantifier__" + quantifierId)
    MethodInvocation[Post](
      ThisObject[Post](this.succ[Class[Post]](cls))(expr.o),
      newMethod.ref,
      args,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty
    )(null)
  }


  final def createQuantifierMethod(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre])(implicit origin: Origin = expr.o): MethodInvocation[Post] = {
    val quantifierId = CodeStringQuantifierMethod.nextId()
    val newBindings = bindings.map(b => variables.succeed(b, b.rewrite()))
    val extraArgsCall = extraArgs.map(v => variables.freeze.computeSucc(v).get).map(v=>v.get)
    val copyOfExtraArgs = extraArgs.map(ea => variables.succeed(ea, ea.rewrite()))
    val nextArguments = bindings ++ extraArgs
    val postStatements: Seq[Statement[Post]] = dispatchPostBody(expr, bindings, left, right, quantifierId, newBindings)
    val newBodyStatements = Seq(createBodyQuantifier(expr, bindings, left, right, nextArguments)) ++ postStatements
    val methodBlock = Block[Post](newBodyStatements)
    val newMethod = declareNewMethod(expr, quantifierId, copyOfExtraArgs, newBindings, methodBlock)
    createMethodCall(expr, quantifierId, newMethod, extraArgsCall)
  }

  /**
   * Creates a quantifier method and creates a method call
   *
   * @param quantifier
   * @param bindings
   * @param body
   * @return
   */
  final def dispatchQuantifier(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], body: Expr[Pre]): Expr[Post] = {
    variables.collectScoped {
      body match {
        case imp: Implies[Pre] => createQuantifierMethod(quantifier, bindings, imp.left, imp.right)
        case and: And[Pre] => createQuantifierMethod(quantifier, bindings, and.left, and.right)
        case _ => super.dispatch(quantifier)
      }
    }._2
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case quantifier: Starall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Exists[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Forall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case p: Perm[Pre] => PermissionRewriter(this)(program).rewritePermission(p)
      case _ => super.dispatch(e)
    }
  }

}
