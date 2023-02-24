package vct.col.rewrite.veymont


import vct.col.ast.{And, Branch, Expr, Loop, Node, Statement, VeyMontCondition, VeyMontThread}
import vct.col.ref.Ref
import vct.col.rewrite.veymont.AddVeyMontAssignmentNodes.{getDerefsFromExpr, getThreadDeref}
import vct.col.rewrite.veymont.AddVeyMontConditionNodes.AddVeyMontConditionError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object AddVeyMontConditionNodes  extends RewriterBuilder {
  override def key: String = "addVeyMontConditionNodes"

  override def desc: String = "Add nodes for VeyMont conditions"

  case class AddVeyMontConditionError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontConditionError"

    override def text: String = node.o.messageInContext(msg)
  }
}

case class AddVeyMontConditionNodes[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(st: Statement[Pre]): Statement[Post] = st match {
    case Branch(branches) => {
      val postbranches = branches.map{case (c,s) => rewriteBranch(c,s)}
      Branch(postbranches)(st.o)
    }
    case l@Loop(init,cond,update,contract,body) => {
      val (postcond,postbody) = rewriteBranch(cond,body)
      Loop(rewriteDefault(init),postcond,rewriteDefault(update),rewriteDefault(contract),postbody)(l.o)
    }
    case _ => rewriteDefault(st)
  }

  private def rewriteBranch(cond: Expr[Pre], st: Statement[Pre]) : (VeyMontCondition[Post],Statement[Post]) = {
    val condMap = getConditionMap(cond)
    val vc = VeyMontCondition[Post](condMap.toList)(st.o)
    (vc, rewriteDefault(st))
  }

  private def getConditionMap(e: Expr[Pre]): Map[Ref[Post, VeyMontThread[Post]], Expr[Post]] = {
    val condEls = collectConditionElements(e)
    val derefs = condEls.map(el => (getDerefsFromExpr(el),el))
    derefs.foldRight(Map.empty[Ref[Post, VeyMontThread[Post]], Expr[Post]]) { case ((d, el), m) =>
      if (d.size != 1)
        throw AddVeyMontConditionError(e, "Conditions of if/while need to reference exactly one thread!")
      else {
        val thread = getThreadDeref(d.head, n => AddVeyMontConditionError(n, "Conditions of if/while can only reference threads, so nothing else!"))
        m + (succ(thread) -> succ(el))
      }
    }
  }

  private def collectConditionElements(e: Expr[Pre]) : List[Expr[Pre]] = e match {
    case And(left,right) => collectConditionElements(left) ++ collectConditionElements(right)
    case _ => List(e)
  }

}
