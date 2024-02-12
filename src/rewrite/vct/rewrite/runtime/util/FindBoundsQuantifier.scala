package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer


case class FindBoundsQuantifier[Pre <: Generation](outer: AbstractQuantifierRewriter[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes


  val bounds: ScopedStack[ArrayBuffer[(Variable[Pre], Option[Expr[Pre]], Option[Expr[Pre]])]] = new ScopedStack()


  def findBounds(expr: Expr[Pre]): ArrayBuffer[(Variable[Pre], Option[Expr[Pre]], Option[Expr[Pre]])] = {
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


  def dispatchAmbiguousGreater(expr: (Expr[Pre], Expr[Pre]))(implicit origin: Origin): Unit = {
    implicit val origin: Origin = expr._1.o
    expr match {
      case (local: Local[Pre], local2: Local[Pre]) => {
        bounds.top.addOne((local.ref.decl, Some(local2 + const(1)), None))
        bounds.top.addOne((local2.ref.decl, None, Some(local)))
      }
      case (local: Local[Pre], e: Expr[Pre]) => bounds.top.addOne((local.ref.decl, Some(e + const(1)), None))
      case (e: Expr[Pre], local: Local[Pre]) => bounds.top.addOne((local.ref.decl, None, Some(e)))
      case _ =>
    }
  }

  def dispatchAmbiguousGreaterEqual(expr: (Expr[Pre], Expr[Pre]))(implicit origin: Origin): Unit = {
    expr match {
      case (local: Local[Pre], local2: Local[Pre]) => {
        bounds.top.addOne((local.ref.decl, Some(local2), None))
        bounds.top.addOne((local2.ref.decl, None, Some(local + const(1))))
      }
      case (local: Local[Pre], e: Expr[Pre]) => bounds.top.addOne((local.ref.decl, Some(e), None))
      case (e: Expr[Pre], local: Local[Pre]) => bounds.top.addOne((local.ref.decl, None, Some(e + const(1))))
      case _ =>
    }
  }

  def dispatchAmbiguous(expr: Expr[Pre])(implicit origin: Origin): Expr[Post] = {
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
    implicit val origin: Origin = e.o
    e match {
      case starAll: Starall[Pre] => dispatchStarAll(starAll)
      case exists: Exists[Pre] => dispatchExists(exists)
      case forAll: Forall[Pre] => dispatchForAll(forAll)
      case _: AmbiguousGreater[Pre] | _: AmbiguousLess[Pre] | _: AmbiguousGreaterEq[Pre] | _: AmbiguousLessEq[Pre] => dispatchAmbiguous(e)
      case _ => super.dispatch(e)
    }
  }
}
