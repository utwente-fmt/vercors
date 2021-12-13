package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{PointsTo, TResource, Type}

trait PointsToImpl[G] { this: PointsTo[G] =>
  override def t: Type[G] = TResource()
}