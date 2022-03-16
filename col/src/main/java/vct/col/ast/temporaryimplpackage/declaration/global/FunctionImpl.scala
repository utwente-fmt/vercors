package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.{Function, PinnedDecl}
import vct.col.ast.temporaryimplpackage.declaration.category.AbstractFunctionImpl

trait FunctionImpl[G] extends GlobalDeclarationImpl[G] with AbstractFunctionImpl[G] { this: Function[G] =>
  def isPin(p: PinnedDecl[G]) = pin.contains(p)
}