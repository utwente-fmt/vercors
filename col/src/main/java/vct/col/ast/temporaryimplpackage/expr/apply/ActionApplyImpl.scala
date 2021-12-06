package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ActionApply, TProcess, Type}

trait ActionApplyImpl { this: ActionApply =>
  override def t: Type = TProcess()
}