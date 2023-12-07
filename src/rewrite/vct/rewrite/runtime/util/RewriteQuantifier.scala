package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{And, Expr, Variable, _}
import vct.col.origin.Origin
import vct.col.ref.LazyRef
import vct.col.resolve.lang.C.o
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CodeStringDefaults._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet


case class RewriteQuantifier[Pre <: Generation](outer: Rewriter[Pre], cls: Class[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  val newVariables: ScopedStack[SuccessionMap[Variable[Pre], Variable[Post]]] = new ScopedStack()
  val requiredLocals: ScopedStack[mutable.HashSet[Variable[Pre]]] = new ScopedStack()
  val allBinders: ArrayBuffer[Variable[Pre]] = new ArrayBuffer()


  def generateNewVariable(variable: Variable[Pre]): Variable[Post] = {
    val newOrigin = Origin(Seq.empty).addPrefName(variable.o.getPreferredNameOrElse() + "_runtime")
    val newType = TInt[Post]()(newOrigin)
    val newVariable: Variable[Post] = new Variable[Post](newType)(newOrigin)
    newVariables.top.update(variable, newVariable)
    newVariable
  }

  def defineLoopCondition(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    val loopCondition = (Not[Post](dispatch(condition))(expr.o), Continue[Post](None)(expr.o))
    Branch[Post](Seq(loopCondition))(expr.o)
  }

  def defineLoopAssertion(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    val loopAssertion = expr match {
      case _: Forall[Pre] => (Not[Post](dispatch(condition))(expr.o), Return[Post](BooleanValue(false)(expr.o))(expr.o))
      case _: Starall[Pre] => (Not[Post](dispatch(condition))(expr.o), Return[Post](BooleanValue(false)(expr.o))(expr.o)) //TODO fix separation conjunction that it keeps track of it if it is the same variable or not
      case _: Exists[Pre] => (dispatch(condition), Return[Post](BooleanValue(true)(expr.o))(expr.o))
      case _ => ???
    }
    Branch[Post](Seq(loopAssertion))(expr.o)
  }

  def createBody(expr: Expr[Pre], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    val loopConditionBranch = defineLoopCondition(expr, left)
    val loopAssertionBranch = defineLoopAssertion(expr, right)
    Block[Post](Seq(loopConditionBranch, loopAssertionBranch))(expr.o)
  }

  def createQuantifier(expr: Expr[Pre], acc: Statement[Post], element: Variable[Pre], filteredBounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])]): CodeStringQuantifier[Post] = {
    CodeStringQuantifier[Post](
      Local[Post](newVariables.top.ref(element))(expr.o),
      filteredBounds.map(i => i._2).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MinValue)(expr.o)),
      filteredBounds.map(i => i._3).collectFirst { case Some(value: Expr[Post]) => value }.getOrElse(IntegerValue[Post](MaxValue)(expr.o)),
      acc
    )(expr.o)
  }

  def createBodyQuantifier(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre]): Statement[Post] = {
    val bounds: ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])] = FindBoundsQuantifier[Pre](this, newVariables.top).findBounds(expr)
    val loopBody = createBody(expr, left, right)
    bindings.reverse.foldLeft[Statement[Post]](loopBody)((acc, element) =>
      createQuantifier(expr, acc, element, bounds.filter(i => i._1 == element))
    )
  }

  def declareNewMethod(expr: Expr[Pre], quantifierId: String, arguments: Seq[Variable[Post]], newLocals: Seq[Variable[Post]], methodBlock: Block[Post]): CodeStringQuantifierMethod[Post] = {
    val newMethodOrigin = expr.o.addPrefName("__runtime_quantifier__" + quantifierId)
    val newMethod = new CodeStringQuantifierMethod[Post](
      quantifierId,
      arguments,
      Some(Scope[Post](newLocals, methodBlock)(expr.o))
    )(null)(newMethodOrigin)
    classDeclarations.declare(newMethod)
    newMethod
  }

  def createMethodCall(expr: Expr[Pre], quantifierId: String, newMethod: CodeStringQuantifierMethod[Post], args: Seq[Expr[Post]]): CodeStringQuantifierCall[Post] = {
    CodeStringQuantifierCall[Post](
      ThisObject[Post](this.succ[Class[Post]](cls))(expr.o),
      quantifierId,
      newMethod.ref,
      args
    )(null)(expr.o)
  }


  def createNewArguments() : Seq[Variable[Pre]] = {
    val requiredArguments = requiredLocals.top --= allBinders
    requiredArguments.toSeq
  }


  def createQuantifierMethod(expr: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], prev: SuccessionMap[Variable[Pre], Variable[Post]]): CodeStringQuantifierCall[Post] = {
    val quantifierId = CodeStringQuantifierMethod.nextId()
    val newLocals: Seq[Variable[Post]] = bindings.map(generateNewVariable)
    val newBodyStatements: Seq[Statement[Post]] = expr match {
      case _: Forall[Pre] => Seq(createBodyQuantifier(expr, bindings, left, right), Return[Post](BooleanValue(true)(expr.o))(expr.o))
      case _: Starall[Pre] => Seq(createBodyQuantifier(expr, bindings, left, right), Return[Post](BooleanValue(true)(expr.o))(expr.o))
      case _: Exists[Pre] => Seq(createBodyQuantifier(expr, bindings, left, right), Return[Post](BooleanValue(false)(expr.o))(expr.o))
      case _ => ???
    }
    allBinders.addAll(bindings)
    val methodBlock = Block[Post](newBodyStatements)(expr.o)
    val arguments: Seq[Variable[Pre]] = createNewArguments()
    val newMethod = declareNewMethod(expr, quantifierId, arguments.map(a => newVariables.top.get(a).get), newLocals, methodBlock)
    createMethodCall(expr, quantifierId, newMethod, arguments.map(a => Local[Post](prev.ref(a))(expr.o)))
  }

  def dispatchQuantifier(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], body: Expr[Pre]): Expr[Post] = {
    val prev = if (newVariables.nonEmpty) newVariables.top else new SuccessionMap[Variable[Pre], Variable[Post]]()
    val result: (Expr[Post], mutable.HashSet[Variable[Pre]]) = requiredLocals.having(new mutable.HashSet[Variable[Pre]]()){
      newVariables.having(new SuccessionMap()){
          val newQuantifier = body match {
            case imp: Implies[Pre] => createQuantifierMethod(quantifier, bindings, imp.left, imp.right, prev)
            case and: And[Pre] => createQuantifierMethod(quantifier, bindings, and.left, and.right, prev)
            case _ => super.dispatch(quantifier)
        }
        (newQuantifier, requiredLocals.top)
      }
    }
    if(requiredLocals.nonEmpty) {
      val d = requiredLocals.top
      requiredLocals.top.addAll(result._2)
    }
    result._1
  }

  def dispatchLocal(local: Local[Pre]): Local[Post] = {
    requiredLocals.top.addOne(local.ref.decl)
    newVariables.top.get(local.ref.decl) match {
      case Some(v) => Local[Post](v.ref)(v.o)
      case None => {
        generateNewVariable(local.ref.decl)
        Local[Post](newVariables.top.ref(local.ref.decl))(local.o)
      }
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case quantifier: Starall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Exists[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case quantifier: Forall[Pre] => dispatchQuantifier(quantifier, quantifier.bindings, quantifier.body)
      case local: Local[Pre] => dispatchLocal(local)
      case _ => super.dispatch(e)
    }
  }
}
