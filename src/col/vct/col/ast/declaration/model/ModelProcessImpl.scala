package vct.col.ast.declaration.model

import vct.col.ast.{ModelProcess, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._
import vct.col.ast.ops.ModelProcessOps

trait ModelProcessImpl[G] extends ModelProcessOps[G] { this: ModelProcess[G] =>
  override def returnType: Type[G] = TProcess()
  override def body: Option[Node[G]] = Some(impl)
  override def check(context: CheckContext[G]): Seq[CheckError] =
    impl.checkSubType(TProcess()) ++ requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())

  override def layout(implicit ctx: Ctx): Doc =
    DocUtil.clauses("requires", requires) <+/>
      (if (modifies.nonEmpty) Text("modifies") <+> Doc.args(modifies.map(ctx.name).map(Text)) <> ";" <> Line else Empty) <>
      (if (accessible.nonEmpty) Text("accessible") <+> Doc.args(accessible.map(ctx.name).map(Text)) <> ";" <> Line else Empty) <>
      DocUtil.clauses("ensures", ensures) <+/>
      Group(Group(Text("process") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ") =") <+> impl)
}