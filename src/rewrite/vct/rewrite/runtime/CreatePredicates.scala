package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.RewriteContractExpr
import vct.col.ast.RewriteHelpers._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CreatePredicates extends RewriterBuilder {
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreatePredicates[Pre <: Generation]() extends Rewriter[Pre] {


  val givenStatementBuffer: mutable.Buffer[Statement[Rewritten[Pre]]] = new ArrayBuffer()
  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }


  override def rewriteDefault(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => currentClass.having(cls){super.dispatch(cls)}
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    val newExpr = new RewriteContractExpr[Pre](this, givenStatementBuffer, currentClass.top).dispatch(e)
    newExpr
  }
}