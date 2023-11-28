package vct.col.ast.statement.veymont

import vct.col.ast.Communicate
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CommunicateOps

trait CommunicateImpl[G] extends CommunicateOps[G] { this: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("communicate") <+> receiver.show <+> "<-" <+> sender.show <> ";"
}
