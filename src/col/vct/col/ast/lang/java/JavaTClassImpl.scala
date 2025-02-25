package vct.col.ast.lang.java

import vct.col.ast.JavaTClass
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.JavaTClassOps

trait JavaTClassImpl[G] extends JavaTClassOps[G] {
  this: JavaTClass[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <>
      (if (typeArgs.isEmpty)
         Empty
       else
         Empty <+> "<" <> Doc.args(typeArgs) <> ">")
}
