package vct.col.ast.lang

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, JavaConstructor}
import vct.col.print._

trait JavaConstructorImpl[G] extends Declarator[G] { this: JavaConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
  override def isStatic = false

  def layoutTypeArgs(implicit ctx: Ctx): Doc =
    if(typeParameters.isEmpty) Empty else Text("<") <> Doc.args(typeParameters) <> ">"

  def layoutSignals(implicit ctx: Ctx): Doc =
    if(signals.isEmpty) Empty else Empty <>> Group(Text("throws") <+> Doc.args(signals))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Doc.spread(modifiers :+ layoutTypeArgs :+ Text(name)) <> "(" <> Doc.args(parameters) <> ")" <> layoutSignals) <+> body.layoutAsBlock,
    ))
}