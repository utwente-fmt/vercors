package vct.col.ast.lang.Isar

import vct.col.ast.IsarDataConstructor
import vct.col.ast.lang.Isar.IsarDoc._
import vct.col.ast.ops.IsarDataConstructorOps
import vct.col.print.{Ctx, Doc, Text}

trait IsarDataConstructorImpl[G] extends IsarDataConstructorOps[G] {
  this: IsarDataConstructor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(this.name) <+> Doc.spread(this.signature.dropRight(1).map { t =>
      inner { t.show }
    })
}
