package vct.col.ast.declaration.cls

import vct.col.ast.{BipConstructor, Declaration, Type, Variable}

trait BipConstructorImpl[G] { this: BipConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = args
}
