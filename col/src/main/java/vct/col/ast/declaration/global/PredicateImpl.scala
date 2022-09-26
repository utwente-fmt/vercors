package vct.col.ast.declaration.global

import vct.col.ast.Predicate
import vct.col.ast.declaration.category.AbstractPredicateImpl

trait PredicateImpl[G] extends GlobalDeclarationImpl[G] with AbstractPredicateImpl[G] { this: Predicate[G] =>

}