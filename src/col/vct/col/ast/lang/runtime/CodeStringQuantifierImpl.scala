package vct.col.ast.lang.runtime

import vct.col.ast.{CodeStringQuantifier, Local}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CodeStringQuantifierOps
trait CodeStringQuantifierImpl[G] extends CodeStringQuantifierOps[G] {
  this: CodeStringQuantifier[G] =>

  def getName(implicit ctx: Ctx): Doc =
    quantifier match {
      case Local(ref) => Text(ctx.name(ref))
      case _ => quantifier.show
    }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("for(") <> getName <+> Text("=") <+> this.lowerBound.show <> Text(";")
      <+> getName <+> Text("<") <+> this.condition.show <> Text(";")
      <+> getName <> Text("++)") <+> body.layoutAsBlock)

}