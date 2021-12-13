package vct.col.ast.temporaryimplpackage.declaration.cls

import vct.col.ast.InstanceMethod
import vct.col.ast.temporaryimplpackage.declaration.category.AbstractMethodImpl

trait InstanceMethodImpl[G] extends ClassDeclarationImpl[G] with AbstractMethodImpl[G] { this: InstanceMethod[G] =>

}