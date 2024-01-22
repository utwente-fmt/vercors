package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter}

import scala.collection.mutable.ArrayBuffer


case class FindBoundsQuantifier[Pre <: Generation](outer: AbstractQuantifierRewriter[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes


  val bounds: ScopedStack[ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])]] = new ScopedStack()


  def findBounds(expr: Expr[Pre]): ArrayBuffer[(Variable[Pre], Option[Expr[Post]], Option[Expr[Post]])] = {
    bounds.having(new ArrayBuffer()) {
      this.dispatch(expr)
      bounds.top
    }
  }

  def dispatchQuantifierBody(expr: Expr[Pre]): Unit = {
    expr match {
      case imp: Implies[Pre] => dispatch(imp.left)
      case _ =>
    }
  }


  def dispatchStarAll(starAll: Starall[Pre]): Starall[Post] = {
    dispatchQuantifierBody(starAll.body)
    starAll.rewrite()
  }

  def dispatchExists(exists: Exists[Pre]): Exists[Post] = {
    dispatchQuantifierBody(exists.body)
    exists.rewrite()
  }

  def dispatchForAll(forAll: Forall[Pre]): Forall[Post] = {
    dispatchQuantifierBody(forAll.body)
    forAll.rewrite()
  }


  def dispatchAmbiguousGreater(expr: (Expr[Pre], Expr[Pre])): Unit = {
    expr match {
      case (local: Local[Pre], local2: Local[Pre]) => {
        bounds.top.addOne((local.ref.decl, Some(AmbiguousPlus[Post](dispatch(local2), IntegerValue[Post](1)(local.o))(null)(local.o)), None))
        bounds.top.addOne((local2.ref.decl, None, Some(dispatch(local))))
      }
      case (local: Local[Pre], e: Expr[Pre]) => bounds.top.addOne((local.ref.decl, Some(AmbiguousPlus[Post](dispatch(e), IntegerValue[Post](1)(local.o))(null)(local.o)), None))
      case (e: Expr[Pre], local: Local[Pre]) => bounds.top.addOne((local.ref.decl, None, Some(dispatch(e))))
      case _ =>
    }
  }

  def dispatchAmbiguousGreaterEqual(expr: (Expr[Pre], Expr[Pre])): Unit = {
    expr match {
      case (local: Local[Pre], local2: Local[Pre]) => {
        bounds.top.addOne((local.ref.decl, Some(dispatch(local2)), None))
        bounds.top.addOne((local2.ref.decl, None, Some(AmbiguousPlus[Post](dispatch(local), IntegerValue[Post](1)(local.o))(null)(local.o))))
      }
      case (local: Local[Pre], e: Expr[Pre]) => bounds.top.addOne((local.ref.decl, Some(dispatch(e)), None))
      case (e: Expr[Pre], local: Local[Pre]) => bounds.top.addOne((local.ref.decl, None, Some(AmbiguousPlus[Post](dispatch(e), IntegerValue[Post](1)(local.o))(null)(local.o))))
      case _ =>
    }
  }

  def dispatchAmbiguous(expr: Expr[Pre]): Expr[Post] = {
    expr match {
      case ag: AmbiguousGreater[Pre] => dispatchAmbiguousGreater((ag.left, ag.right))
      case al: AmbiguousLess[Pre] => dispatchAmbiguousGreater((al.right, al.left))
      case age: AmbiguousGreaterEq[Pre] => dispatchAmbiguousGreaterEqual((age.left, age.right))
      case ale: AmbiguousLessEq[Pre] => dispatchAmbiguousGreaterEqual((ale.right, ale.left))
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
