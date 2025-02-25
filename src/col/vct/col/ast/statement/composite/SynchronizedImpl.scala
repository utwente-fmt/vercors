package vct.col.ast.statement.composite

import vct.col.ast.Synchronized
import vct.col.print._
import vct.col.ast.ops.SynchronizedOps

trait SynchronizedImpl[G] extends SynchronizedOps[G] {
  this: Synchronized[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("synchronized") <+> "(" <> Doc.arg(obj) <> ")") <+>
      body.layoutAsBlock
}
