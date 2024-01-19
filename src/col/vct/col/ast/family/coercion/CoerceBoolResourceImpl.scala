package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoolResource, TResource}
import vct.col.ast.ops.CoerceBoolResourceOps

trait CoerceBoolResourceImpl[G] extends CoerceBoolResourceOps[G] { this: CoerceBoolResource[G] => 
  override def target: TResource[G] = TResource()
}
