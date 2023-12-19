package vct.col.ast.statement.composite

import vct.col.ast.{TNothing, TryCatchFinally, Type, Block}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError, RedundantCatchClause}
import vct.col.print._
import vct.col.typerules.Types
import vct.col.ast.ops.TryCatchFinallyOps

trait TryCatchFinallyImpl[G] extends NodeFamilyImpl[G] with TryCatchFinallyOps[G] { this: TryCatchFinally[G] =>
  def checkOverlappingCatches: Seq[CheckError] = {
    this.catches.foldLeft[Type[G]](TNothing()) {
      case (caughtAlready, clause) =>
        if(caughtAlready.superTypeOf(clause.decl.t)) {
          return Seq(RedundantCatchClause(clause))
        } else {
          Types.leastCommonSuperType(caughtAlready, clause.decl.t)
        }
    }
    Nil
  }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ checkOverlappingCatches

  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(Seq(
      Text("try") <+> body.layoutAsBlock,
      Doc.spread(catches),
      if(after == Block[G](Nil)) Empty else Text("finally") <+> after.layoutAsBlock
    ))
}