package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorFunction, Variable}
import vct.col.print._

trait InstanceOperatorFunctionImpl[G] { this: InstanceOperatorFunction[G] =>
  def typeArgs: Seq[Variable[G]] = Nil

  override def layout(implicit ctx: Ctx): Doc = Group(
    Text("pure") <+> returnType <+> operator <> "(" <> Doc.args(args) <> ")" <>
      body.map(Text(" =") <>> _).getOrElse(Text(";"))
  )
}
