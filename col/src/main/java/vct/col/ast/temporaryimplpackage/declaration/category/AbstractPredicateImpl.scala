package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{AbstractPredicate, TResource, Type}

trait AbstractPredicateImpl extends InlineableApplicableImpl { this: AbstractPredicate =>
  def threadLocal: Boolean
  override def returnType: Type = TResource()
}
