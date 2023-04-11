package vct.col.ast.lang

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, PVLConstructor}
import vct.col.print.{Ctx, Doc, Text, Empty}

trait PVLConstructorImpl[G] extends Declarator[G] { this: PVLConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ contract.givenArgs ++ contract.yieldsArgs

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Text("constructor") <> "(" <> Doc.args(args) <> ")" <> body.map(Empty <+> _.layoutAsBlock).getOrElse(Text(";"))
    ))
}