package vct.col.ast.`type`

import vct.col.ast.TConst
import vct.col.ast.ops.TConstOps
import vct.col.print._

trait TConstImpl[G] extends TConstOps[G] { this: TConst[G] =>
   override def layout(implicit ctx: Ctx): Doc = Text("const") <+> inner
}
