package vct.col.ast.declaration.category

import vct.col.ast.InlineableApplicable

trait InlineableApplicableImpl[G] extends FilterApplicableImpl[G] {
  this: InlineableApplicable[G] =>
  def inline: Boolean
}
