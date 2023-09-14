package vct.col.ast.`type`

import vct.col.ast.TUnion
import vct.col.print.{Ctx, Doc}

trait TUnionImpl[G] { this: TUnion[G] =>
  override def layout(implicit ctx: Ctx): Doc =  Doc.fold(types)(_ <+> "|" <+> _)

}