package vct.col.ast.declaration.cls

import vct.col.ast.InstanceMethod
import vct.col.ast.declaration.category.AbstractMethodImpl
import vct.col.print._

trait InstanceMethodImpl[G] extends ClassDeclarationImpl[G] with AbstractMethodImpl[G] { this: InstanceMethod[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    returnType.show <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")" <>
      body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";"))
}