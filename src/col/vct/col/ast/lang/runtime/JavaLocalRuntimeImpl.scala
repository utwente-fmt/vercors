package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}

trait JavaLocalRuntimeImpl[G] { this: JavaLocalRuntime[G] =>
  override lazy val t: Type[G] = ref.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(this.o.getPreferredNameOrElse())
}