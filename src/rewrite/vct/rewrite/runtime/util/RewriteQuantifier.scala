package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{Variable, _}
import vct.col.ref.LazyRef
import vct.col.resolve.lang.C.o
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.rewrite.runtime.util.CodeStringDefaults._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class RewriteQuantifier[Pre <: Generation](outer: Rewriter[Pre], cls: Class[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def dispatchStarAll(starAll: Starall[Pre]): Starall[Post] = {
    //TODO
    starAll.rewrite()
  }

  def dispatchExists(exists: Exists[Pre]): Exists[Post] = {
    //TODO
    exists.rewrite()
  }

  def createBodyForAllQuantifier(forAll: Forall[Pre], imp: Implies[Pre]): Statement[Post] = {
    //TODO not completely correct, should be another if statement that checks if the loop condition is correct and then in that if statement should be another if statement that checks if the right side of the implementation is correct.
    val bounds: ArrayBuffer[(Variable[Pre], Int, Int)] = FindBoundsQuantifier[Pre]().findBounds(forAll)
    val loopCondition = Not[Post](dispatch(imp.right))(forAll.o)
    val loopConditionBody = Return[Post](BooleanValue(false)(forAll.o))(forAll.o)
    val loopBranch = Branch[Post](Seq((loopCondition, loopConditionBody)))(forAll.o)
    val allForLoops = forAll.bindings.reverse.foldLeft[Statement[Post]](loopBranch)((acc, element) => {
      val filteredBounds = bounds.filter(i => i._1 == element)
      val quantifier = new CodeStringQuantifier(
        element.rewrite(),
        filteredBounds.map(i => i._2).max,
        filteredBounds.map(i => i._3).min,
        Seq(acc)
      )(forAll.o)
      quantifier
    })
    allForLoops
  }


  def createQuantifierMethod(forAll: Forall[Pre], imp: Implies[Pre]): CodeStringQuantifierCall[Post] = {
    val quantifierId = CodeStringQuantifierMethod.nextId()
    val newLocals: Seq[Variable[Post]] = forAll.bindings.map(v => v.rewrite())
    val methodBlock = Block[Post](
      Seq(
        createBodyForAllQuantifier(forAll, imp),
        Return[Post](BooleanValue(true)(forAll.o))(forAll.o)
      ))(forAll.o)

    val newMethod = new CodeStringQuantifierMethod[Post](
      quantifierId,
      Seq.empty,
      Some(Scope[Post](newLocals, methodBlock)(forAll.o))
    )(null)(forAll.o)
    classDeclarations.declare(newMethod)

    CodeStringQuantifierCall[Post](
      ThisObject[Post](this.succ[Class[Post]](cls))(forAll.o),
      quantifierId,
      newMethod.ref,
      Seq.empty
    )(null)(forAll.o)
  }


  def dispatchForAll(forAll: Forall[Pre]): Expr[Post] = {
    forAll.body match {
      case imp: Implies[Pre] => createQuantifierMethod(forAll, imp)
      case _ => forAll.rewrite()
    }

  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case starAll: Starall[Pre] => dispatchStarAll(starAll)
      case exists: Exists[Pre] => dispatchExists(exists)
      case forAll: Forall[Pre] => dispatchForAll(forAll)
      case _ => super.dispatch(e)
    }
  }
}
