package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorMethod, Type, Variable}
import vct.col.print._

trait InstanceOperatorMethodImpl[G] { this: InstanceOperatorMethod[G] =>
  def typeArgs: Seq[Variable[G]] = Nil
  def outArgs: Seq[Variable[G]] = Nil

  override def layout(implicit ctx: Ctx): Doc = Group(
    returnType.show <+> operator <> "(" <> Doc.args(args) <> ")" <>
      body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";"))
  )
}
