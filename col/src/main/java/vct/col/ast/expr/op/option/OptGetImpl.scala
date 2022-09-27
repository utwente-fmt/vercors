package vct.col.ast.expr.op.option

import vct.col.ast.{OptGet, Type}

trait OptGetImpl[G] { this: OptGet[G] =>
  override def t: Type[G] = opt.t.asOption.get.element
}