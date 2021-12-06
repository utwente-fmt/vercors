package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverResource, TResource, Type}

trait SilverResourceImpl { this: SilverResource =>
  override def t: Type = TResource()
}