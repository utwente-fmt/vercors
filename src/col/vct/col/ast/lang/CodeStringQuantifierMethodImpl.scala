package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}

trait CodeStringQuantifierMethodImpl[G] {
  this: CodeStringQuantifierMethod[G] =>

  override def pure: Boolean = false
  override def inline: Boolean = false
  override def outArgs: Seq[Variable[G]] = Seq.empty
  override def contract: ApplicableContract[G] = null
  override def typeArgs: Seq[Variable[G]] = Seq.empty

  override def returnType: Type[G] = TBool[G]()


  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Text("public boolean __runtime__")))

}