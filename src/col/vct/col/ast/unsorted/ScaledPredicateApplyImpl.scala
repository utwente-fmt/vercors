package vct.col.ast.unsorted

import vct.col.ast.{ScaledPredicateApply, WritePerm}
import vct.col.ast.ops.ScaledPredicateApplyOps
import vct.col.print._

trait ScaledPredicateApplyImpl[G] extends ScaledPredicateApplyOps[G] { this: ScaledPredicateApply[G] =>
   override def layout(implicit ctx: Ctx): Doc =
     perm match {
       case WritePerm() =>
         apply.show
       case perm =>
         Group(Text("[") <> Doc.arg(perm) <> "]" <> apply)
     }
}
