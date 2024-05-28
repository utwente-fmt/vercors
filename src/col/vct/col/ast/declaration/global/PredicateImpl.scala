package vct.col.ast.declaration.global

import vct.col.ast.Predicate
import vct.col.ast.declaration.category.AbstractPredicateImpl
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.PredicateOps

trait PredicateImpl[G]
    extends GlobalDeclarationImpl[G]
    with AbstractPredicateImpl[G]
    with PredicateOps[G] {
  this: Predicate[G] =>
  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("predicate") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")" <>
      (if (body.nonEmpty)
         Text(" {") <>> body.get <+/> "}"
       else
         Empty)

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(inline -> "inline", threadLocal -> "thread_local").filter(_._1)
      .values.map(Text).map(Doc.inlineSpec).toSeq

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Doc.rspread(layoutModifiers) <> "resource" <+> ctx.name(this) <> "(" <>
        Doc.args(args) <> ")" <> body.map(Text(" =") <>> _.show)
          .getOrElse(Empty)
    )

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.spec(Show.lazily(layoutSpec(_)))
    }
}
