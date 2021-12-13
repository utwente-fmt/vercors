package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ProcessApply, TProcess, Type}

trait ProcessApplyImpl[G] { this: ProcessApply[G] =>
  override def t: Type[G] = TProcess()
}