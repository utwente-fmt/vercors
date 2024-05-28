package vct.col.ast.declaration.cls

import vct.col.ast.{BipConstructor, Declaration, Type, Variable}
import vct.col.ast.ops.BipConstructorOps

trait BipConstructorImpl[G] extends BipConstructorOps[G] {
  this: BipConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = args
}
