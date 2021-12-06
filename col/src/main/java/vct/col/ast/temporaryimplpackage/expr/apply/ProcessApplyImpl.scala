package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ProcessApply, TProcess, Type}

trait ProcessApplyImpl { this: ProcessApply =>
  override def t: Type = TProcess()
}