package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CodeStringQuantifierMethodOps
import vct.col.util.AstBuildHelpers

trait CodeStringQuantifierMethodImpl[G] extends CodeStringQuantifierMethodOps[G] {
  this: CodeStringQuantifierMethod[G] =>

  override def pure: Boolean = false
  override def inline: Boolean = false
  override def outArgs: Seq[Variable[G]] = Seq.empty
  override def contract: ApplicableContract[G] = AstBuildHelpers.contract(this.o)
  override def typeArgs: Seq[Variable[G]] = Seq.empty

  override def returnType: Type[G] = TBool[G]()


  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Group(Text(s"public boolean __runtime_quantifier__${this.quantifierId}") <>
        "(" <> Doc.args(args) <> ")") <>
        body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";")),
    )))

}