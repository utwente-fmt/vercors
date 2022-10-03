package vct.col.ast.declaration.cls

import vct.col.ast.InstancePredicate
import vct.col.ast.declaration.category.AbstractPredicateImpl

trait InstancePredicateImpl[G] extends ClassDeclarationImpl[G] with AbstractPredicateImpl[G] { this: InstancePredicate[G] =>

}