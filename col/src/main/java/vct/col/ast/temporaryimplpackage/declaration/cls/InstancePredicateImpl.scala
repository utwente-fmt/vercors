package vct.col.ast.temporaryimplpackage.declaration.cls

import vct.col.ast.InstancePredicate
import vct.col.ast.temporaryimplpackage.declaration.category.AbstractPredicateImpl

trait InstancePredicateImpl[G] extends ClassDeclarationImpl[G] with AbstractPredicateImpl[G] { this: InstancePredicate[G] =>

}