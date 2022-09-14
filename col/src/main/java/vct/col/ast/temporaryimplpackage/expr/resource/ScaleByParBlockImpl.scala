package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{ScaleByParBlock, Type}

trait ScaleByParBlockImpl[G] { this: ScaleByParBlock[G] =>
  override def t: Type[G] = res.t
}
