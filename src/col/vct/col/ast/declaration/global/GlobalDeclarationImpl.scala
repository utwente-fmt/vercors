package vct.col.ast.declaration.global

import vct.col.ast.GlobalDeclaration
import vct.col.ast.ops.GlobalDeclarationFamilyOps

trait GlobalDeclarationImpl[G] extends GlobalDeclarationFamilyOps[G] { this: GlobalDeclaration[G] =>

}