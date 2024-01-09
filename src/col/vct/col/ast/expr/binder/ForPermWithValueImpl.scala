package vct.col.ast.expr.binder

import vct.col.ast.{ForPermWithValue, TBool, Variable}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.ForPermWithValueOps

trait ForPermWithValueImpl[G] extends ForPermWithValueOps[G] { this: ForPermWithValue[G] =>
  override def bindings: Seq[Variable[G]] = Seq(binding)
  override def t: TBool[G] = TBool()

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("(\\forpermwithvalue") <+> binding <> ";" <>> body.show </> ")")
}
