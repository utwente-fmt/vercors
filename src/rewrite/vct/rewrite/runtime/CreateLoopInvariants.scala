package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.RewriteContractExpr

object CreateLoopInvariants extends RewriterBuilder{
  override def key: String = "createLoopInvariants"

  override def desc: String = "Create loop invariant statements"
}


case class CreateLoopInvariants[Pre<: Generation]() extends Rewriter[Pre]{


  implicit var program: Program[Pre] = null
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



  def dispatchLoopContract(lc: LoopContract[Pre]) : Seq[Statement[Post]] = {
    lc match {
      case li: LoopInvariant[Pre] => {
        val contract = li.invariant
        val (_, newStatements) = new RewriteContractExpr[Pre](this, currentClass.top)(program, null).createStatements(contract)
        newStatements.toSeq
      }
      case _ => Seq.empty
    }
  }


  def dispatchLoop(l: Loop[Pre]): Statement[Post] = {
    val loopContract = dispatchLoopContract(l.contract)
    super.dispatch(l)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case l: Loop[Pre] => dispatchLoop(l)
      case _ => super.dispatch(stat)
    }
  }
}