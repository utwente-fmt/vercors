package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{TResource, Type, Wand}

trait WandImpl { this: Wand =>
  override def t: Type = TResource()
}