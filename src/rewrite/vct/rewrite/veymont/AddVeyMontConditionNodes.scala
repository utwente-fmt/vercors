package vct.col.rewrite.veymont


import hre.util.ScopedStack
import vct.col.ast.{And, Block, BooleanValue, Branch, Declaration, Expr, Loop, Node, Statement, VeyMontCondition, SeqProg, Endpoint}
import vct.col.ref.Ref
import vct.col.rewrite.veymont.AddVeyMontAssignmentNodes.{getDerefsFromExpr, getThreadDeref}
import vct.col.rewrite.veymont.AddVeyMontConditionNodes.AddVeyMontConditionError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

import scala.reflect.internal.util.TriState.True

object AddVeyMontConditionNodes  extends RewriterBuilder {
  override def key: String = "addVeyMontConditionNodes"

  override def desc: String = "Add nodes for VeyMont conditions"

  case class AddVeyMontConditionError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontConditionError"

    override def text: String = node.o.messageInContext(msg)
  }
}

case class AddVeyMontConditionNodes[Pre <: Generation]() extends Rewriter[Pre] {

  val inSeqProg: ScopedStack[Int] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case dcl: SeqProg[Pre] =>
        inSeqProg.push(dcl.threads.size)
        try {
          rewriteDefault(dcl)
        } finally {
          inSeqProg.pop()
        }
      case _ => rewriteDefault(decl)
    }

  override def dispatch(st: Statement[Pre]): Statement[Post] = {
    if(inSeqProg.nonEmpty) {
      st match {
      case Branch(branches) => {
        val postbranches = branches.map{case (c,s) => rewriteBranch(c,s)}
        Branch(postbranches)(st.o)
      }
      case l: Loop[Pre] =>
          rewriteLoop(l)
      case _ => rewriteDefault(st)
      }
    } else rewriteDefault(st)
  }

  private def rewriteBranch(cond: Expr[Pre], st: Statement[Pre]) : (Expr[Post],Statement[Post]) = {
    val condMap = checkConditionAndGetConditionMap(cond)
    if(condMap.isEmpty) //in case of else statement
      (dispatch(cond),dispatch(st))
    else (VeyMontCondition[Post](condMap.toList)(st.o), dispatch(st))
  }

  private def rewriteLoop(l: Loop[Pre]): Loop[Post] = {
    val condMap = checkConditionAndGetConditionMap(l.cond)
    if (condMap.isEmpty)
      throw AddVeyMontConditionError(l.cond, "Conditions of loops cannot be `true'!")
    else  Loop(rewriteDefault(l.init),
      VeyMontCondition[Post](condMap.toList)(l.cond.o),
      rewriteDefault(l.update),
      rewriteDefault(l.contract),
      dispatch(l.body))(l.o)
  }

  private def checkConditionAndGetConditionMap(e: Expr[Pre]): Map[Ref[Post, Endpoint[Post]], Expr[Post]] = {
    if (isTrue(e))
      Map.empty
    else {
      val condEls = collectConditionElements(e)
      val m = getConditionMap(condEls,e)
      if(m.keys.toSet.size == inSeqProg.top) {
        m
      } else throw AddVeyMontConditionError(e, "Conditions of if/while need to reference each thread exactly once!")
    }
  }

  private def getConditionMap(condEls: List[Expr[Pre]], e : Expr[Pre]): Map[Ref[Post, Endpoint[Post]], Expr[Post]] = {
    val derefs = condEls.map(el => (getDerefsFromExpr(el), el))
    derefs.foldRight(Map.empty[Ref[Post, Endpoint[Post]], Expr[Post]]) { case ((d, el), m) =>
      if (d.size != 1)
        throw AddVeyMontConditionError(e, "Conditions of if/while need to reference each thread exactly once!")
      else {
        val thread = getThreadDeref(d.head, AddVeyMontConditionError(e, "Conditions of if/while can only reference threads, so nothing else!"))
        m + (succ(thread) -> dispatch(el))
      }
    }
  }

  private def isTrue(e : Expr[Pre]) = e match {
    case BooleanValue(value) => value
    case _ => false
  }

  private def collectConditionElements(e: Expr[Pre]) : List[Expr[Pre]] = e match {
    case And(left,right) => collectConditionElements(left) ++ collectConditionElements(right)
    case _ => List(e)
  }

}
