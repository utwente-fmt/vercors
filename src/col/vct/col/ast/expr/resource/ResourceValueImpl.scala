package vct.col.ast.expr.resource

import vct.col.ast.{ResourceValue, TResourceVal}

trait ResourceValueImpl[G] { this: ResourceValue[G] =>
  override def t: TResourceVal[G] = TResourceVal()
}
