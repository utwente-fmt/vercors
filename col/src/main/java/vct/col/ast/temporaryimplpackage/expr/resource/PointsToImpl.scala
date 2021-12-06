package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{PointsTo, TResource, Type}

trait PointsToImpl { this: PointsTo =>
  override def t: Type = TResource()
}