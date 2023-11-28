package vct.col.ast.statement.exceptional

import vct.col.ast.{Return, Void}
import vct.col.print.{Ctx, Doc, Text, Empty}
import vct.col.ast.ops.ReturnOps

trait ReturnImpl[G] extends ReturnOps[G] { this: Return[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("return") <> (if(result == Void[G]()) Text(";") else Empty <+> result <> ";")
}