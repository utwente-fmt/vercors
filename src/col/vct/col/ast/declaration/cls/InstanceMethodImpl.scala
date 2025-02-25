package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceMethod, TVoid}
import vct.col.ast.declaration.category.AbstractMethodImpl
import vct.col.check.{
  CheckContext,
  CheckError,
  SeqProgInstanceMethodArgs,
  SeqProgInstanceMethodBody,
  SeqProgInstanceMethodNonVoid,
  SeqProgInstanceMethodPure,
}
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.InstanceMethodOps

trait InstanceMethodImpl[G]
    extends ClassDeclarationImpl[G]
    with AbstractMethodImpl[G]
    with InstanceMethodOps[G] {
  this: InstanceMethod[G] =>
  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(pure -> "pure", inline -> "inline").filter(_._1).values.map(Text)
      .map(Doc.inlineSpec).toSeq

  private def layoutNameAndArgs(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java =>
        (if (typeArgs.nonEmpty)
           Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <+> Empty
         else
           Empty) <> ctx.name(this)
      case _ =>
        Text(ctx.name(this)) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text))
           else
             Empty)
    }

  def layoutPvl(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Group(
          Doc.rspread(layoutModifiers) <> returnType <+> layoutNameAndArgs
        ) <> "(" <> Doc.args(args) <> ")"
      ) <>
        (if (outArgs.nonEmpty)
           Text(" returns") <+> "(" <> Doc.args(outArgs) <> ")"
         else
           Empty) <> body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";")),
    ))

  def layoutJava(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Group(
          Doc.rspread(layoutModifiers) <> returnType <+> layoutNameAndArgs
        ) <> "(" <> Doc.args(args) <> ")"
      ) <>
        (if (outArgs.nonEmpty)
           Text(" returns") <+> "(" <> Doc.args(outArgs) <> ")"
         else
           Empty) <> body.map(Text(" ") <> _.layoutAsBlock)
          .getOrElse(Text(" { throw new UnsupportedOperationException(); }")),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => layoutPvl
    }
}
