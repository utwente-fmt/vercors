package vct.col.ast.expr.heap.read

import vct.col.ast.ops.PointerBlockOps
import vct.col.ast.{PointerBlock, TPointerBlock, Type}
import vct.col.print._

trait PointerBlockImpl[G] extends PointerBlockOps[G] {
  this: PointerBlock[G] =>
  override def t: Type[G] = TPointerBlock()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_block(") <> pointer <> ")"
}
