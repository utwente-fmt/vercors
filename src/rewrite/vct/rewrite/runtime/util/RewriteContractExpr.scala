package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.serialize
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.RewriteContractExpr.quantifiers

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object RewriteContractExpr{

  val quantifiers = new ArrayBuffer[String]

}


case class RewriteContractExpr[Pre <: Generation](outer: Rewriter[Pre], givenStatementBuffer: mutable.Buffer[Statement[Rewritten[Pre]]]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes




  val bounds: ScopedStack[ArrayBuffer[Tuple3[Variable[Pre], Int, Int]]] = ScopedStack()
  val internalCodeStrings: ScopedStack[mutable.Buffer[String]] = ScopedStack()

  def dispatchStar(star: Star[Pre]): Star[Post] = {
    //TODO maybe?
    star.rewrite()
  }

  /**
   *Ideas for creating quantifiers:
   *
   * If the quantifier has multiple binding the for loops can be nested in each other in the same function (this will increase the performance
   *
   * Steps of each quantifier:
   * 2. create method call for the quantifier in a code statement
   */


  def dispatchStarAll(starAll: Starall[Pre]): Starall[Post] = {
    //TODO
    starAll.rewrite()
  }

  def dispatchExists(exists: Exists[Pre]): Exists[Post] = {
    //TODO
    exists.rewrite()
  }

  def dispatchForAll(forAll: Forall[Pre]): Forall[Post] = {
    forAll.body match {
      case imp: Implies[Pre] =>
        bounds.having(new ArrayBuffer[Tuple3[Variable[Pre], Int, Int]]()) {
          bounds.top.addAll(forAll.bindings.map(b => (b,MinValue,MaxValue)))
          val methodName = internalCodeStrings.having(new ArrayBuffer[String]()){
            super.dispatch(forAll)
            val body = internalCodeStrings.top.mkString("\n")
            val loopCondition = loopConditionTemplate(imp.left.toString, body)
            val forLoopBody = forAll.bindings.reverse.foldLeft[String](loopCondition)((acc, element) => {
              val filteredBounds = bounds.top.filter(i => i._1 == element)
              val lowerBound = filteredBounds.map(i => i._2).max
              val upperBound = filteredBounds.map(i => i._3).min
              quantifierTemplate(element.o.getPreferredNameOrElse(), lowerBound, upperBound, acc)
            })

            val newMethodId = quantifiers.size.toString
            quantifiers.addOne(newMethodId)
            val newMethod = CodeStringClass(methodTemplate(newMethodId, newMethodId), forLoopBody)(forAll.o)
            classDeclarations.declare(newMethod)
            newMethodId
          }

          if(internalCodeStrings.nonEmpty) {
            internalCodeStrings.top.addOne()
          }
        }
      case _ =>
    }
    forAll.rewrite()
  }


  def dispatchAmbiguousBounds(input: Tuple2[_, _], adder: Int = 0): Unit = {
    if (bounds.nonEmpty) {
      input match {
        case (iv: IntegerValue[Pre], local: Local[Pre]) => bounds.top.addOne(local.ref.decl, MinValue, iv.toString.toInt - adder)
        case (local: Local[Pre], iv: IntegerValue[Pre]) => bounds.top.addOne(local.ref.decl, iv.toString.toInt + adder, MaxValue)
        case _ =>
      }
    }
  }

  def dispatchAmbiguous(expr: Expr[Pre]): Expr[Post] = {
    expr match {
      case ag: AmbiguousGreater[Pre] => dispatchAmbiguousBounds((ag.left, ag.right), 1)
      case al: AmbiguousLess[Pre] => dispatchAmbiguousBounds((al.right, al.left), 1)
      case age: AmbiguousGreaterEq[Pre] => dispatchAmbiguousBounds((age.left, age.right))
      case ale: AmbiguousLessEq[Pre] => dispatchAmbiguousBounds((ale.right, ale.left))
      case _ => ??? //Impossible
    }
    super.dispatch(expr)
  }

  def dispatchDefault(expr: Expr[Pre]): Expr[Post] = {
    givenStatementBuffer.addOne(CodeStringStatement[Post](assertCondition(expr.toString))(expr.o))
    super.dispatch(expr)
  }


  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case star: Star[Pre] => dispatchStar(star)
      case starAll: Starall[Pre] => dispatchStarAll(starAll)
      case exists: Exists[Pre] => dispatchExists(exists)
      case forAll: Forall[Pre] => dispatchForAll(forAll)
      case _: AmbiguousGreater[Pre] | _: AmbiguousLess[Pre] | _: AmbiguousGreaterEq[Pre] | _: AmbiguousLessEq[Pre] => dispatchAmbiguous(e)
      case _ => dispatchDefault(e)
    }
  }
}
