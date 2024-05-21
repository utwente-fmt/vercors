package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaLocalRuntimeOps

trait JavaLocalRuntimeImpl[G] extends JavaLocalRuntimeOps[G] { this: JavaLocalRuntime[G] =>
  override lazy val t: Type[G] = ref.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(ref.decl.name)
}