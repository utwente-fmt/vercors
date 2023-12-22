package vct.col.ast.lang.java

import vct.col.ast.JavaSharedInitialization
import vct.col.print.{Ctx, Doc, Text, Empty}
import vct.col.ast.ops.JavaSharedInitializationOps

trait JavaSharedInitializationImpl[G] extends JavaSharedInitializationOps[G] { this: JavaSharedInitialization[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    (if(isStatic) Text("static") <+> Empty else Empty) <> initialization.layoutAsBlock
}