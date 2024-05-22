package vct.col.ast.declaration.model

import vct.col.ast.ModelDeclaration
import vct.col.ast.ops.ModelDeclarationFamilyOps

trait ModelDeclarationImpl[G] extends ModelDeclarationFamilyOps[G] { this: ModelDeclaration[G] =>

}
