package vct.col.ast.declaration.cls

import vct.col.ast.BipTransition
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.util.AstBuildHelpers.tt

import scala.util.Try

trait BipTransitionImpl[G] { this: BipTransition[G] =>
  def layoutAnnotation(implicit ctx: Ctx): Doc =
    Group(Text("@Transition(") <> Doc.args(Seq(
      Text("name =") <+> ctx.name(this),
      Text("source =") <+> ctx.name(source),
      Text("target =") <+> ctx.name(target),
      if (requires == tt[G]) Empty else Text("requires =") <+> requires,
    )) <> ")")

  def layoutArgs(implicit ctx: Ctx): Doc =
    Doc.args(data.map { name =>
      Text("@Data(name =") <+> ctx.name(name) <> ")" <+> Try(name.decl.t).getOrElse(Text("?brokenref?")) <+> ctx.name(name)
    })

  override def layout(implicit ctx: Ctx): Doc =
    layoutAnnotation </>
      Group(Text("public void") <+> ctx.name(this) <> "(" <> layoutArgs <> ")") <+> body.layoutAsBlock
}