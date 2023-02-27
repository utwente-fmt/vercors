package vct.col.ast.declaration.global

import vct.col.ast.{Function}
import vct.col.ast.declaration.category.AbstractFunctionImpl

trait FunctionImpl[G] extends GlobalDeclarationImpl[G] with AbstractFunctionImpl[G] { this: Function[G] =>

}
