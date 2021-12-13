package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ApplyInlineable, InlineableApplicable}
import vct.col.ref.Ref

trait ApplyInlineableImpl[G] { this: ApplyInlineable[G] =>
  override def ref: Ref[G, _ <: InlineableApplicable[G]]
}