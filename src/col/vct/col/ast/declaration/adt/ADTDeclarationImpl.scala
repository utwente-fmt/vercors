package vct.col.ast.declaration.adt

import vct.col.ast.ADTDeclaration
import vct.col.ast.ops.ADTDeclarationFamilyOps

trait ADTDeclarationImpl[G] extends ADTDeclarationFamilyOps[G] {
  this: ADTDeclaration[G] =>

}
