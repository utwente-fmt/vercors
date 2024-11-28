package vct.col.ast.declaration.category

import vct.col.ast.{FilterApplicable, FilterMode}

trait FilterApplicableImpl[G] extends ApplicableImpl[G] {
  this: FilterApplicable[G] =>
  def filter: FilterMode[G]
}
