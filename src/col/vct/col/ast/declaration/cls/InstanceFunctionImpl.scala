package vct.col.ast.declaration.cls

import vct.col.ast.InstanceFunction
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.InstanceFunctionOps

trait InstanceFunctionImpl[G] extends InstanceFunctionOps[G] {
  this: InstanceFunction[G] =>
  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(inline -> "inline", threadLocal -> "thread_local").filter(_._1)
      .values.map(Text).map(Doc.inlineSpec).toSeq

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.rspread(layoutModifiers) <> Text("pure") <+> returnType <+>
          ctx.name(this) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">"
           else
             Empty) <> "(" <> Doc.args(args) <> ")" <>
          body.map(Text(" =") <>> _ <> ";").getOrElse(Text(";"))
      ),
    ))
}
