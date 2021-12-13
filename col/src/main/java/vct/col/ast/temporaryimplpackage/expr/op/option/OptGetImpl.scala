package vct.col.ast.temporaryimplpackage.expr.op.option

import vct.col.ast.{OptGet, Type}

trait OptGetImpl[G] { this: OptGet[G] =>
  override def t: Type[G] = opt.t.asOption.get.element
}