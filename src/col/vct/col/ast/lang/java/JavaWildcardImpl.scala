package vct.col.ast.lang.java

import vct.col.ast.JavaWildcard
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaWildcardOps

trait JavaWildcardImpl[G] extends JavaWildcardOps[G] { this: JavaWildcard[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("?")
}
