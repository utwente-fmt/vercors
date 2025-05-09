package vct.col.ast.`type`

import vct.col.ast.TPointerBlock
import vct.col.ast.ops.TPointerBlockOps
import vct.col.print._
import vct.col.typerules.TypeSize

trait TPointerBlockImpl[G] extends TPointerBlockOps[G] {
  this: TPointerBlock[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("`block`")
}
