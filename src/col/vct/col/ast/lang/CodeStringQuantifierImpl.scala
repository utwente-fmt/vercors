package vct.col.ast.lang

import vct.col.ast.{CodeString, CodeStringQuantifier, Local}
import vct.col.print.{Ctx, Doc, Group, Text}
trait CodeStringQuantifierImpl[G] {
  this: CodeStringQuantifier[G] =>



  def getName: Doc = {
    this.quantifier match {
      case Local(ref) => Text(ref.decl.o.getPreferredNameOrElse())
      case _ => Text(this.quantifier.toString)
    }
  }


  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("for(") <> getName <+> Text("=") <+> this.lowerBound.show<> Text(";")
      <+> getName <+> Text("<") <+> this.condition.show <> Text(";")
      <+> getName <> Text("++)") <+> body.layoutAsBlock)

}