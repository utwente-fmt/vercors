package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.Predicate
import vct.col.ast.temporaryimplpackage.declaration.category.AbstractPredicateImpl

trait PredicateImpl[G] extends GlobalDeclarationImpl[G] with AbstractPredicateImpl[G] { this: Predicate[G] =>

}