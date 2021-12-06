package vct.col.ast.temporaryimplpackage.declaration.adt

import vct.col.ast.temporaryimplpackage.declaration.category.ApplicableImpl
import vct.col.ast.{ADTFunction, Node}

trait ADTFunctionImpl extends ApplicableImpl with ADTDeclarationImpl { this: ADTFunction =>
  override def body: Option[Node] = None
}