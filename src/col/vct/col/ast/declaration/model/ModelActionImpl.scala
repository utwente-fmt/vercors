package vct.col.ast.declaration.model

import vct.col.ast.{ModelAction, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait ModelActionImpl[G] {
  this: ModelAction[G] =>
  override def returnType: Type[G] = TProcess()
  override def body: Option[Node[G]] = None

  override def check(context: CheckContext[G]): Seq[CheckError] =
    requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())

  override def layout(implicit ctx: Ctx): Doc =
    DocUtil.clauses("requires", requires) <+/>
      (if (modifies.nonEmpty)
         Text("modifies") <+> Doc.args(modifies.map(ctx.name).map(Text)) <>
           ";" <> Line
       else
         Empty) <>
      (if (accessible.nonEmpty)
         Text("accessible") <+> Doc.args(accessible.map(ctx.name).map(Text)) <>
           ";" <> Line
       else
         Empty) <> DocUtil.clauses("ensures", ensures) <+/>
      Group(Text("action") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ");")
}
