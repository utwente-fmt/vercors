package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, VeyMontSeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._

trait VeyMontSeqProgImpl[G] extends Declarator[G] { this: VeyMontSeqProg[G] =>
  def members: Seq[Declaration[G]] = threads ++ Seq(runMethod) ++ methods
  override def declarations: Seq[Declaration[G]] = progArgs ++ members

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(progArgs) <> ")") <+> "{" <>>
        Doc.stack(threads ++ methods :+ runMethod) <+/>
      "}"
    ))
}
