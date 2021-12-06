package vct.col.ast.temporaryimplpackage.expr.op.option

import vct.col.ast.{OptGet, Type}

trait OptGetImpl { this: OptGet =>
  override def t: Type = opt.t.asOption.get.element
}