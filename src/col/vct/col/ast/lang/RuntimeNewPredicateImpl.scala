package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}

trait RuntimeNewPredicateImpl[G] {
  this: RuntimeNewPredicate[G] =>

  def getClassType(implicit ctx: Ctx): Doc = Doc.arg(instance.t)

  override def layout(implicit ctx: Ctx): Doc =
    Group(getClassType <+> this.o.getPreferredNameOrElse() <+> Text("= new ") <+> getClassType <> Text("();"))

}