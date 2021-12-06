package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.InlineableApplicable

trait InlineableApplicableImpl extends ApplicableImpl { this: InlineableApplicable =>
  def inline: Boolean
}
