package vct.col.ast.declaration.cls

import vct.col.ast.InstancePredicate
import vct.col.ast.declaration.category.AbstractPredicateImpl
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.InstancePredicateOps

trait InstancePredicateImpl[G] extends ClassDeclarationImpl[G] with AbstractPredicateImpl[G] with InstancePredicateOps[G] { this: InstancePredicate[G] =>
  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] = ListMap(
    inline -> "inline",
    threadLocal -> "thread_local",
  ).filter(_._1).values.map(Text).map(Doc.inlineSpec).toSeq

  override def layout(implicit ctx: Ctx): Doc =
    Doc.spec(Group(
      Doc.rspread(layoutModifiers) <> "resource" <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")" <>
        body.map(Text(" =") <>> _ <> ";").getOrElse(Text(";"))
    ))
}