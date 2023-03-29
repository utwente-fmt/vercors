package vct.col.ast.declaration.category

import vct.col.ast.{AbstractPredicate, TResource, Type}

trait AbstractPredicateImpl[G] extends InlineableApplicableImpl[G] { this: AbstractPredicate[G] =>
  def threadLocal: Boolean
  override def returnType: Type[G] = TResource()
}
