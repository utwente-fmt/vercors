package vct.col.ast.declaration.cls

import vct.col.ast.{RunMethod, TVoid}
import vct.col.print._
import vct.col.ast.ops.RunMethodOps

trait RunMethodImpl[G] extends RunMethodOps[G] {
  this: RunMethod[G] =>

  def layoutJava(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract.show,
      Text("public") <+> TVoid().show <+> "run" <> "()" <+>
        body.map(_.layoutAsBlock).getOrElse(Text("{ /*@ assume false; @*/ }")),
    ))

  def layoutPvl(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract.show,
      Text("run") <> body.map(Empty <+> _.layoutAsBlock).getOrElse(Text(";")),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => layoutPvl
    }
}
