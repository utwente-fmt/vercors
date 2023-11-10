package vct.rewrite.veymont

import vct.col.ast._
import vct.col.origin.PanicBlame
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

object EncodeSeqBranchUnanimity  extends RewriterBuilder {
  override def key: String = "encodeBranchUnanimity"
  override def desc: String = "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in seq_program nodes."

  case class AddVeyMontConditionError(node : Node[_], msg: String) extends UserError {
    override def code: String = "addVeyMontConditionError"
    override def text: String = node.o.messageInContext(msg)
  }
}

case class EncodeSeqBranchUnanimity[Pre <: Generation]() extends Rewriter[Pre] {

  val guardSucc = SuccessionMap[SeqGuard[Pre], Variable[Post]]()

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case SeqBranch(guards, yes, no) =>
      implicit val o = statement.o
      /*
      for each c in conds:
        bool bc = c;

      for each subsequent c1, c2 in conds:
        assert c1 == c2

      boolean cond = foldStar(succ(c))

      if (cons) dispatch(yes) dispatch(no)
       */
      val assignments: Seq[Assign[Post]] = guards.map { guard =>
        guardSucc(guard) = new Variable(TBool())
        assignLocal(guardSucc(guard).get, dispatch(guard.condition))
      }

      val assertions: Seq[Assert[Post]] = (0 until guards.length - 1).map { i =>
        val c1 = guards(i)
        val c2 = guards(i + 1)
        Assert(guardSucc(c1).get === guardSucc(c2).get)(PanicBlame("TODO: Implement failing assert"))
      }

      val majorCond = new Variable[Post](TBool())
      val majorAssign: Assign[Post] = assignLocal[Post](
        majorCond.get,
        foldAnd(guards.map(guard => guardSucc(guard).get))
      )

      val finalIf: Branch[Post] = Branch[Post](Seq(
        (majorCond.get, dispatch(yes)),
        (tt, no.map(dispatch).getOrElse(Block(Seq())))
      ))

      Scope(guards.map(guardSucc(_)) :+ majorCond, Block(
        assignments ++
        assertions :+
        majorAssign :+
        finalIf
      ))

    case statement => rewriteDefault(statement)
  }
}
