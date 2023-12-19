package vct.col.ast.family.location

import vct.col.ast.ArrayLocation
import vct.col.print._

trait ArrayLocationImpl[G] { this: ArrayLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(array.bind(Precedence.POSTFIX) <> "[" <> Doc.arg(subscript) <> "]")
}
