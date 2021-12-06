package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ApplyInlineable, InlineableApplicable}
import vct.col.ref.Ref

trait ApplyInlineableImpl { this: ApplyInlineable =>
  override def ref: Ref[_ <: InlineableApplicable]
}