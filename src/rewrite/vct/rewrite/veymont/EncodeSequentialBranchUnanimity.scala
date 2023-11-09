package vct.rewrite.veymont


import hre.util.ScopedStack
import vct.col.ast.{And, Block, BooleanValue, Branch, Declaration, Expr, Loop, Node, Statement, SeqGuard, SeqProg, Endpoint}
import vct.col.ref.Ref
import vct.rewrite.veymont.EncodeSequentialBranchUnanimity.AddVeyMontConditionError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._

object EncodeSequentialBranchUnanimity  extends RewriterBuilder {
  override def key: String = "encodeBranchUnanimity"
  override def desc: String = "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in seq_program nodes."

  case class AddVeyMontConditionError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontConditionError"
    override def text: String = node.o.messageInContext(msg)
  }
}

case class EncodeSequentialBranchUnanimity[Pre <: Generation]() extends Rewriter[Pre] {

  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
//
//  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
//    case prog: SeqProg[Pre] => currentProg.having(prog) {
//      rewriteDefault(prog)
//    }
//    case decl => rewriteDefault(decl)
//  }
//
//  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
//    case branch: Branch[Pre] if currentProg.nonEmpty =>
//      branch.rewrite(branch.branches.map { case (c, s) => rewriteBranch(c, s) })
//
//    case l: Loop[Pre] if currentProg.nonEmpty =>
//      val condMap = checkConditionAndGetConditionMap(l.cond)
//      if (condMap.isEmpty)
//        throw AddVeyMontConditionError(l.cond, "Conditions of loops cannot be `true'!")
//      else Loop(
//        rewriteDefault(l.init),
//        SeqGuard[Post](condMap.toList)(l.cond.o),
//        rewriteDefault(l.update),
//        rewriteDefault(l.contract),
//        dispatch(l.body)
//      )(l.o)
//
//    case statement => rewriteDefault(statement)
//  }
//
//  private def rewriteBranch(cond: Expr[Pre], st: Statement[Pre]) : (Expr[Post],Statement[Post]) = {
//    val condMap = checkConditionAndGetConditionMap(cond)
//    if(condMap.isEmpty) //in case of else statement
//      (dispatch(cond),dispatch(st))
//    else (SeqGuard[Post](condMap.toList)(st.o), dispatch(st))
//  }
//
//  private def checkConditionAndGetConditionMap(e: Expr[Pre]): Map[Ref[Post, Endpoint[Post]], Expr[Post]] = e match {
//    case BooleanValue(true) => Map.empty
//    case BooleanValue(_) =>
//      val condEls = unfoldStar(e)
//      val m = getConditionMap(condEls,e)
//      if(m.keys.toSet.size == currentSeqProg.top) {
//        m
//      } else
//        // TODO: Is this requirement properly handled?
////        throw AddVeyMontConditionError(e, "Conditions of if/while need to reference each thread exactly once!")
//  }
//
//  private def getConditionMap(condEls: Seq[Expr[Pre]], e : Expr[Pre]): Map[Ref[Post, Endpoint[Post]], Expr[Post]] = {
//    val derefs = condEls.map(el => (getDerefsFromExpr(el), el))
//    derefs.foldRight(Map.empty[Ref[Post, Endpoint[Post]], Expr[Post]]) { case ((d, el), m) =>
//      if (d.size != 1)
//        throw AddVeyMontConditionError(e, "Conditions of if/while need to reference each thread exactly once!")
//      else {
//        val thread = getThreadDeref(d.head, AddVeyMontConditionError(e, "Conditions of if/while can only reference threads, so nothing else!"))
//        m + (succ(thread) -> dispatch(el))
//      }
//    }
//  }
}
