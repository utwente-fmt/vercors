package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{TResource, Type, Wand}

trait WandImpl[G] { this: Wand[G] =>
  override def t: Type[G] = TResource()
}