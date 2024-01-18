package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.{And, Expr, Variable, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.SuccessionMap

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


abstract class AbstractQuantifierRewriter[Pre <: Generation](val outer: Rewriter[Pre], val cls: Class[Pre], val firstRequiredLocals: Seq[Variable[Pre]] = Seq.empty)(implicit program: Program[Pre], newVariables: NewVariableGenerator[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  val requiredLocals: ScopedStack[mutable.HashSet[Variable[Pre]]] = new ScopedStack()
  val allBinders: ArrayBuffer[Variable[Pre]] = new ArrayBuffer()



  def dispatchLoopBody(quantifier: Expr[Pre], left: Expr[Pre], right: Expr[Pre]): Seq[Statement[Post]] = Seq()

  def dispatchPostBody(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], quantifierId: String, newLocals: Seq[Variable[Post]]) : Seq[Statement[Post]] = Seq()


  final def defineLoopCondition(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    val loopCondition = (Not[Post](dispatch(condition))(expr.o), Continue[Post](None)(expr.o))
    Branch[Post](Seq(loopCondition))(expr.o)
  }



  final def createBody(expr: Expr[Pre], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    val loopConditionBranch = defineLoopCondition(expr, left)
    val dispatchedLoopBody = dispatchLoopBody(expr, left, right)
    Block[Post](Seq(loopConditionBranch) ++ dispatchedLoopBody)(expr.o)
  }

  final def createQuantifier(expr: Expr[Pre], acc: Statement[Post], element: Variable[Pre], filteredBounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])]): CodeStringQuantifier[Post] = {
    implicit val origin: Origin = expr.o
    CodeStringQuantifier[Post](
      newVariables.getLocal(element),
      filteredBounds.map(i => i._2).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MinValue)),
      filteredBounds.map(i => i._3).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MaxValue)),
      acc
    )(expr.o)
  }

  final def createBodyQuantifier(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    val bounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])] = FindBoundsQuantifier[Pre](this).findBounds(expr)
    val loopBody = createBody(expr, left, right)
    bindings.reverse.foldLeft[Statement[Post]](loopBody)((acc, element) =>
      createQuantifier(expr, acc, element, bounds.filter(i => i._1 == element))
    )
  }

  final def declareNewMethod(expr: Expr[Pre], quantifierId: String, arguments: Seq[Variable[Post]], newLocals: Seq[Variable[Post]], methodBlock: Block[Post]): CodeStringQuantifierMethod[Post] = {
    val newMethodOrigin = expr.o.addPrefName("__runtime_quantifier__" + quantifierId)
    val newMethod = new CodeStringQuantifierMethod[Post](
      quantifierId,
      arguments,
      Some(Scope[Post](newLocals, methodBlock)(expr.o))
    )(null)(newMethodOrigin)
    classDeclarations.declare(newMethod)
    newMethod
  }

  final def createMethodCall(expr: Expr[Pre], quantifierId: String, newMethod: CodeStringQuantifierMethod[Post], args: Seq[Expr[Post]]): CodeStringQuantifierCall[Post] = {
    CodeStringQuantifierCall[Post](
      ThisObject[Post](this.succ[Class[Post]](cls))(expr.o),
      quantifierId,
      newMethod.ref,
      args
    )(null)(expr.o)
  }


  final def createNewArguments() : Seq[Variable[Pre]] = {
    val requiredArguments = requiredLocals.top --= allBinders
    firstRequiredLocals.map(newVariables.getOrCreate)
    requiredArguments.toSeq ++ firstRequiredLocals
  }


  final def createQuantifierMethod(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], prev: NewVariableResult[Pre, _]): CodeStringQuantifierCall[Post] = {
    implicit val origin: Origin = expr.o
    val quantifierId = CodeStringQuantifierMethod.nextId()
    val newLocals: Seq[Variable[Post]] = bindings.map(newVariables.createNew)
    val postStatements: Seq[Statement[Post]] = dispatchPostBody(expr, bindings, left, right, quantifierId, newLocals)
    val newBodyStatements: Seq[Statement[Post]] = Seq(createBodyQuantifier(expr, bindings, left, right)) ++ postStatements
    allBinders.addAll(bindings)
    val methodBlock = Block[Post](newBodyStatements)
    val arguments: Seq[Variable[Pre]] = createNewArguments()
    val newMethod = declareNewMethod(expr, quantifierId, arguments.map(a => newVariables.get(a).get), newLocals, methodBlock)
    createMethodCall(expr, quantifierId, newMethod, arguments.map(a => prev.getLocal(a)))
  }

  final def dispatchQuantifier(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], body: Expr[Pre]): Expr[Post] = {
    val prev = newVariables.freeze()
    val result: (Expr[Post], mutable.HashSet[Variable[Pre]]) = requiredLocals.having(new mutable.HashSet[Variable[Pre]]()){
      newVariables.collect{
          val newQuantifier = body match {
            case imp: Implies[Pre] => createQuantifierMethod(quantifier, bindings, imp.left, imp.right, prev)
            case and: And[Pre] => createQuantifierMethod(quantifier, bindings, and.left, and.right, prev)
            case _ => super.dispatch(quantifier)
        }
        (newQuantifier, requiredLocals.top)
      }
    }.result
    if(requiredLocals.nonEmpty) {
      requiredLocals.top.addAll(result._2)
    }
    result._1
  }

  final def dispatchLocal(local: Local[Pre]): Local[Post] = {
    requiredLocals.top.addOne(local.ref.decl)
    newVariables.getLocal(local)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case quantifier: Starall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Exists[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Forall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case local: Local[Pre] => dispatchLocal(local)
      case p: Perm[Pre] => PermissionRewriter(this)(program, newVariables).rewritePermission(p)
      case _ => super.dispatch(e)
    }
  }
}
