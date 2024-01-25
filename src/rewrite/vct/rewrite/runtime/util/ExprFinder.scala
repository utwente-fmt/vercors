package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case class ExprFinder[Pre <: Generation, ExprType <: Expr[Pre]]()(implicit tag: ClassTag[ExprType]) extends Rewriter[Pre] {

  val dereferences: ArrayBuffer[ExprType] = new ArrayBuffer[ExprType]()

  def collect(e: Expr[Pre]) : Seq[ExprType] = {
    this.dispatch(e)
    dereferences.toSeq
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case a: ExprType => dereferences.addOne(a)
      case _ =>
    }
    super.dispatch(e)
  }
}
