package vct.col.ast.declaration.singular

import vct.col.ast.{Endpoint, TClass, Type}
import vct.col.print._

trait EndpointImpl[G] { this: Endpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("endpoint") <+> ctx.name(this) <+> "=" <>> { Group(t.show <> "(" <> Doc.args(args) <> ");") })

  def t: Type[G] = TClass(cls)
}
