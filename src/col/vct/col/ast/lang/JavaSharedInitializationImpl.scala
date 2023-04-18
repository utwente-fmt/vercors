package vct.col.ast.lang

import vct.col.ast.JavaSharedInitialization
import vct.col.print.{Ctx, Doc, Text, Empty}

trait JavaSharedInitializationImpl[G] { this: JavaSharedInitialization[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    (if(isStatic) Text("static") <+> Empty else Empty) <> initialization.layoutAsBlock
}