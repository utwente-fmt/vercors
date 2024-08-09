package vct.col.ast.lang.c

import vct.col.ast.CUnique
import vct.col.ast.ops.CUniqueOps
import vct.col.print._

trait CUniqueImpl[G] extends CUniqueOps[G] { this: CUnique[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("unique<") <> i.toString() <> ">")
}
