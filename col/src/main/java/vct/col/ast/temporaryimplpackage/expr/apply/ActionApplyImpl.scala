package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ActionApply, TProcess, Type}

trait ActionApplyImpl[G] { this: ActionApply[G] =>
  override def t: Type[G] = TProcess()
}