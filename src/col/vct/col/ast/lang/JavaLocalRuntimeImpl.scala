package vct.col.ast.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx._
import vct.col.typerules.Types

trait JavaLocalRuntimeImpl[G] { this: JavaLocalRuntime[G] =>
  override lazy val t: Type[G] = ref.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(this.o.getPreferredNameOrElse())
}