package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorFunction, Variable}
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.InstanceOperatorFunctionOps

trait InstanceOperatorFunctionImpl[G] extends InstanceOperatorFunctionOps[G] {
  this: InstanceOperatorFunction[G] =>
  def typeArgs: Seq[Variable[G]] = Nil

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(inline -> "inline", threadLocal -> "thread_local").filter(_._1)
      .values.map(Text).map(Doc.inlineSpec).toSeq

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.rspread(layoutModifiers) <> "pure" <+> returnType <+> operator <>
          "(" <> Doc.args(args) <> ")" <> body.map(Text(" =") <>> _ <> ";")
            .getOrElse(Text(";"))
      ),
    ))
}
