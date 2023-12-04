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
  val currentContract: ScopedStack[AccountedPredicate[Pre]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    if(currentClass.isEmpty || currentContract.isEmpty) {
      return super.dispatch(e)
    }
    val newExpr = new RewriteContractExpr[Pre](this, givenStatementBuffer, currentClass.top).dispatch(e)
    newExpr
  }


  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchInstanceMethod(im)
      case cls: Class[Pre] => currentClass.having(cls){super.dispatch(cls)}
      case _ => super.dispatch(decl)
    }
  }

  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => sc.body match {
        case block: Block[Pre] =>
          dispatchMethodBlock(block, im)

          super.dispatch(im)
        case _ => ???
      }
      case _ => super.dispatch(im)
    }
  }

  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Unit = {
    val preConditionStatements = currentContract.having(im.contract.requires){
      dispatch(im.contract.requires)
      val statements = givenStatementBuffer.toSeq
      givenStatementBuffer.clear()
      statements
    }
    //    val postConditionStatements: Seq[CodeStringStatement[Post]] = dispatchApplicableContractToAssert(im.contract.ensures)
  }
}