package vct.col.ast.declaration.cls

import vct.col.ast.{Final, InstanceField}
import vct.col.print._
import vct.col.ast.ops.InstanceFieldOps

trait InstanceFieldImpl[G] extends InstanceFieldOps[G] { this: InstanceField[G] =>
  def isFinal = flags.collectFirst { case _: Final[G] => () }.isDefined

  override def layout(implicit ctx: Ctx): Doc =
    Doc.rspread(flags) <> t.show <+> ctx.name(this) <> ";"
}