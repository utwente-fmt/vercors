package vct.col.ast.expr.resource

import vct.col.ast.{ScaleByParBlock, Type}
import vct.col.ast.ops.ScaleByParBlockOps

trait ScaleByParBlockImpl[G] extends ScaleByParBlockOps[G] { this: ScaleByParBlock[G] =>
  override def t: Type[G] = res.t
}
