package vct.col.ast.statement.exceptional

import vct.col.ast.{Return, Void}
import vct.col.print.{Ctx, Doc, Text, Empty}

trait ReturnImpl[G] { this: Return[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("return") <> (if(result == Void[G]()) Text(";") else Empty <+> result <> ";")
}