package vct.col.ast.lang

import vct.col.ast.{Declaration, JavaMethod, JavaStatic}
import vct.col.ast.util.Declarator
import vct.col.print._

trait JavaMethodImpl[G] extends Declarator[G] { this: JavaMethod[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
  override def isStatic = modifiers.contains(JavaStatic[G]())

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Group(Group(
        Doc.lspread(modifiers) <>
        (if(typeParameters.isEmpty) Empty else Text("<") <> Doc.args(typeParameters) <> ">" <+> Empty) <>
        returnType <+> name <> "[]".repeat(dims)) <> "(" <> Doc.args(parameters) <> ")") <>
        (if(signals.isEmpty) Empty else Empty <>> Group(Text("throws") <+> Doc.args(signals)))
      ) <> body.map(Empty <+> _.layoutAsBlock).getOrElse(Text(";"))
    ))
}