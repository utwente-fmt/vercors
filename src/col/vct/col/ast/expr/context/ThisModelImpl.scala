package vct.col.ast.expr.context

import vct.col.ast.{TModel, ThisModel, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ThisModelOps

trait ThisModelImpl[G] extends ThisDeclarationImpl[G] with ThisModelOps[G] { this: ThisModel[G] =>
  override def t: Type[G] = TModel(cls)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("this")
}