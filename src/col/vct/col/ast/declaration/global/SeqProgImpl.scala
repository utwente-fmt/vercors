package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, SeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._

trait SeqProgImpl[G] extends Declarator[G] { this: SeqProg[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ threads ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(threads ++ decls :+ run) <+/>
      "}"
    ))

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    super.enterCheckContext(context).withSeqProg(this)
}
