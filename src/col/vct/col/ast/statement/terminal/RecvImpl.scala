package vct.col.ast.statement.terminal

import vct.col.ast.Recv
import vct.col.print._
import vct.col.ast.ops.RecvOps

trait RecvImpl[G] extends RecvOps[G] { this: Recv[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("recv") <+> ctx.name(ref) <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(layoutSpec(_)))
}