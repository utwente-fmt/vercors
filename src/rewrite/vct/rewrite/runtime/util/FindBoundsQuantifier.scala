package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.rewrite.runtime.util.CodeStringDefaults._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class FindBoundsQuantifier[Pre <: Generation]() extends Rewriter[Pre] {
  val bounds: ScopedStack[ArrayBuffer[Tuple3[Variable[Pre], Int, Int]]] = new ScopedStack()

  def findBounds(expr: Expr[Pre]): ArrayBuffer[Tuple3[Variable[Pre], Int, Int]] = {
    bounds.having(new ArrayBuffer()){
      this.dispatch(expr)
      bounds.top
    }
  }

  def dispatchStarAll(starAll: Starall[Pre]): Starall[Post] = {
    bounds.top.addAll(starAll.bindings.map(b => (b,MinValue,MaxValue)))
    starAll.body match {
      case imp: Implies[Pre] => dispatch(imp.left)
      case _ =>
    }
    starAll.rewrite()
  }

  def dispatchExists(exists: Exists[Pre]): Exists[Post] = {
    bounds.top.addAll(exists.bindings.map(b => (b,MinValue,MaxValue)))
    exists.body match {
      case imp: Implies[Pre] => dispatch(imp.left)
      case _ =>
    }
    exists.rewrite()
  }

  def dispatchForAll(forAll: Forall[Pre]): Forall[Post] = {
    bounds.top.addAll(forAll.bindings.map(b => (b,MinValue,MaxValue)))
    forAll.body match {
      case imp: Implies[Pre] => dispatch(imp.left)
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


  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case starAll: Starall[Pre] => dispatchStarAll(starAll)
      case exists: Exists[Pre] => dispatchExists(exists)
      case forAll: Forall[Pre] => dispatchForAll(forAll)
      case _: AmbiguousGreater[Pre] | _: AmbiguousLess[Pre] | _: AmbiguousGreaterEq[Pre] | _: AmbiguousLessEq[Pre] => dispatchAmbiguous(e)
      case _ => super.dispatch(e)
    }
  }
}
