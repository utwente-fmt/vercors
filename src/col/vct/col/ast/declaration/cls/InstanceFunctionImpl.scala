package vct.col.ast.declaration.cls

import vct.col.ast.InstanceFunction
import vct.col.print._

trait InstanceFunctionImpl[G] { this: InstanceFunction[G] =>
  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] = Seq(
    if(inline) "inline" else "",
    if(threadLocal) "thread_local" else "",
  ).filter(_.nonEmpty).map(Text)

  override def layout(implicit ctx: Ctx): Doc =
    contract.show <+/> Group(
      Doc.rspread(layoutModifiers) <> Text("pure") <+> returnType <+> ctx.name(this) <>
        (if(typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">" else Empty) <>
        "(" <> Doc.args(args) <> ")" <>
        body.map(Text(" =") <>> _).getOrElse(Text(";"))
    )
}