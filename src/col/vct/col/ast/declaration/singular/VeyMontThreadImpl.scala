package vct.col.ast.declaration.singular

import vct.col.ast.VeyMontThread
import vct.col.print._

trait VeyMontThreadImpl[G] { this: VeyMontThread[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("thread") <+> ctx.name(this) <+> "=" <>> { Group(threadType.show <> "(" <> Doc.args(args) <> ");") })
}
