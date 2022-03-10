package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.{TNothing, TryCatchFinally, Type}
import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError, RedundantCatchClause}
import vct.col.util.Types

trait TryCatchFinallyImpl[G] extends NodeFamilyImpl[G] { this: TryCatchFinally[G] =>
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
}