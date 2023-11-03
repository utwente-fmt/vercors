package vct.col.ast.lang

import vct.col.ast.JavaTClass
import vct.col.print.{Ctx, Doc, Empty, Text}

trait JavaTClassImpl[G] { this: JavaTClass[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <>
      (if(typeArgs.isEmpty) Empty else Empty <+> "<" <> Doc.args(typeArgs) <> ">")
}