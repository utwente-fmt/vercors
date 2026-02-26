package vct.col.ast.family.loopcontract

import vct.col.ast.{IterationContract, Node}
import vct.col.print._
import vct.col.ast.ops.IterationContractOps
import vct.col.check.CheckContext

trait IterationContractImpl[G] extends IterationContractOps[G] {
  this: IterationContract[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      DocUtil.clauses("context_everywhere", context_everywhere),
      DocUtil.clauses("requires", requires),
      DocUtil.clauses("ensures", ensures),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.spec(Show.lazily(layoutSpec(_)))

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] =
    this match {
      case IterationContract(requires, ensures, context_everywhere) =>
        Seq(
          f(context.withPrecondition, requires),
          f(context.withPostcondition, ensures),
          f(context.withPrecondition, context_everywhere),
        )
    }
}
