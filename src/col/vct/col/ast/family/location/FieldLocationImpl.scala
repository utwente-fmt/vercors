package vct.col.ast.family.location

import vct.col.ast.{FieldLocation, Final}
import vct.col.print._
import vct.col.ast.ops.FieldLocationOps
import vct.col.check.{CheckContext, CheckError, FinalPermission}

trait FieldLocationImpl[G] extends FieldLocationOps[G] with LocationImpl[G] { this: FieldLocation[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    if(field.decl.flags.collectFirst { case Final() => () }.nonEmpty) Seq(FinalPermission(this)) ++ super.check(context)
    else super.check(context)

  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
