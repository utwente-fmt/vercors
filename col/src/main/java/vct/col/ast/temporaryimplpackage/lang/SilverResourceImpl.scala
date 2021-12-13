package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverResource, TResource, Type}

trait SilverResourceImpl[G] { this: SilverResource[G] =>
  override def t: Type[G] = TResource()
}