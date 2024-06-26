package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorMethod, Type, Variable}
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.InstanceOperatorMethodOps

trait InstanceOperatorMethodImpl[G] extends InstanceOperatorMethodOps[G] {
  this: InstanceOperatorMethod[G] =>
  def typeArgs: Seq[Variable[G]] = Nil
  def outArgs: Seq[Variable[G]] = Nil

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(pure -> "pure", doInline -> "inline").filter(_._1).values.map(Text)
      .map(Doc.inlineSpec).toSeq

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.rspread(layoutModifiers) <> returnType.show <+> operator <> "(" <>
          Doc.args(args) <> ")"
      ) <> body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";")),
    ))
}
