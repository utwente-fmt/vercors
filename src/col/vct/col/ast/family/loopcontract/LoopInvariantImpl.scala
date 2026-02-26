package vct.col.ast.family.loopcontract

import vct.col.ast.LoopInvariant
import vct.col.print._
import vct.col.ast.ops.LoopInvariantOps
import vct.col.check.CheckContext

trait LoopInvariantImpl[G] extends LoopInvariantOps[G] {
  this: LoopInvariant[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Doc.spec(Doc.stack(Seq(
      Doc.stack(decreases.toSeq),
      DocUtil.clauses(
        if (ctx.syntax == Ctx.Silver)
          "invariant"
        else
          "loop_invariant",
        invariant,
      ),
    )))
  }

  // Count loop invariants as postconditions for checks (allowed to contain old and not allowed to have perm/forperm without \polarity_dependent)
  override def enterCheckContextInPostCondition(
      context: CheckContext[G]
  ): Boolean = true
}
