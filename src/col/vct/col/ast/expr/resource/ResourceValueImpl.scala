package vct.col.ast.expr.resource

import vct.col.ast.{ResourceValue, TResourceVal}
import vct.col.ast.ops.ResourceValueOps

trait ResourceValueImpl[G] extends ResourceValueOps[G] { this: ResourceValue[G] =>
  override def t: TResourceVal[G] = TResourceVal()
}
