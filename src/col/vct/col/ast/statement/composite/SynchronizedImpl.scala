package vct.col.ast.statement.composite

import vct.col.ast.Synchronized
import vct.col.print._

trait SynchronizedImpl[G] { this: Synchronized[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("synchronized") <+> "(" <> Doc.arg(obj) <> ")") <+> body.layoutAsBlock
}