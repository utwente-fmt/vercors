package vct.col.ast.family.loopcontract

import vct.col.ast.LoopInvariant
import vct.col.print._

trait LoopInvariantImpl[G] { this: LoopInvariant[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(decreases.toSeq),
      DocUtil.clauses(if(ctx.syntax == Ctx.Silver) "invariant" else "loop_invariant", invariant),
    ))
}