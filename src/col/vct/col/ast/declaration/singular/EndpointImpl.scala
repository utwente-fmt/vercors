package vct.col.ast.declaration.singular

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{Endpoint, TByReferenceClass, TClass, Type}
import vct.col.print._
import vct.col.ast.ops.{EndpointFamilyOps, EndpointOps}
import vct.col.check.{CheckContext, CheckError}

trait EndpointImpl[G]
    extends EndpointOps[G] with EndpointFamilyOps[G] with DeclarationImpl[G] {
  this: Endpoint[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("endpoint") <+> ctx.name(this) <+> "=" <>> {
      Group(t.show <> "(" <> Doc.args(args) <> ");")
    })

  def t: TClass[G] = TByReferenceClass(cls, typeArgs)

  override def check(ctx: CheckContext[G]): Seq[CheckError] =
    super.check(ctx) ++ ctx.checkInScope(this, cls)

}
