package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteBlock, RewriteLoop}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.RewriteContractExpr
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

object CreateLoopInvariants extends RewriterBuilder {
  override def key: String = "createLoopInvariants"

  override def desc: String = "Create loop invariant statements"
}


case class CreateLoopInvariants[Pre <: Generation]() extends Rewriter[Pre] {


  implicit var program: Program[Pre] = _
  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case _ => super.dispatch(decl)
    }
  }


  def dispatchLoopContract(lc: LoopContract[Pre]): Statement[Post] = {
    lc match {
      case li: LoopInvariant[Pre] => {
        val contract = li.invariant
        val pd: PermissionData[Pre] = PermissionData().setOuter(this).setCls(currentClass.top)
        RewriteContractExpr[Pre](pd).createAssertions(contract)
      }
      case _ => Block[Post](Seq.empty)(lc.o)
    }
  }

  def dispatchLoop(l: Loop[Pre]): Statement[Post] = {
    implicit val o: Origin = l.o
    val loopContract: Statement[Post] = dispatchLoopContract(l.contract)
    val rewrittenBody: Statement[Rewritten[Pre]] = dispatch(l.body)
    val newBody = Block[Post](Seq(loopContract, rewrittenBody, loopContract))
    l.rewrite(body = newBody)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case l: Loop[Pre] => dispatchLoop(l)
      case b: Block[Pre] => dispatchBlock(b)
      case _ => super.dispatch(stat)
    }
  }


  def dispatchBlock(b: Block[Pre]): Block[Post] = {
    b.rewrite()
  }
}