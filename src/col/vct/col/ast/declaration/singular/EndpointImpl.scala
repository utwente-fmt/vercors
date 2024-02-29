package vct.col.ast.declaration.singular

import vct.col.ast.{Endpoint, TClass, Type}
import vct.col.print._
import vct.col.ast.ops.{EndpointOps, EndpointFamilyOps}

trait EndpointImpl[G] extends EndpointOps[G] with EndpointFamilyOps[G] { this: Endpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("endpoint") <+> ctx.name(this) <+> "=" <>> { Group(t.show <> "(" <> Doc.args(args) <> ");") })

  def t: TClass[G] = TClass(cls, typeArgs)
}
