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

  def layoutJava(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.rspread(layoutModifiers) <> Doc.inlineSpec(Text("pure")) <+>
          returnType <+> ctx.name(this) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">"
           else
             Empty) <> "(" <> Doc.args(args) <> ")" <> body.map(body =>
            Text(" ") <> "{" <>>
              // TODO (RR, PB): Why must the "return body" part be parenthesized here?
              (Text("return") <+> Nest(body.show)) <> ";" <+/> "}"
          ).getOrElse(Text(";"))
      ),
    ))

  def layoutPvl(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.rspread(layoutModifiers) <> Doc.inlineSpec(Text("pure")) <+>
          returnType <+> ctx.name(this) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">"
           else
             Empty) <> "(" <> Doc.args(args) <> ")" <>
          body.map(Text(" =") <>> _ <> ";").getOrElse(Text(";"))
      ),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case Ctx.PVL => layoutPvl
    }
}
