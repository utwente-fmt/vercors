package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoolResource, TResource}

trait CoerceBoolResourceImpl[G] { this: CoerceBoolResource[G] => 
  override def target: TResource[G] = TResource()
}
