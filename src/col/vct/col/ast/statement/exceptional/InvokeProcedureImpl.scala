package vct.col.ast.statement.exceptional

import vct.col.ast.{InvokeProcedure, Variable, Type}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Text}
import vct.col.ast.ops.InvokeProcedureOps

trait InvokeProcedureImpl[G] extends InvokeProcedureOps[G] {
  this: InvokeProcedure[G] =>
  def layoutGeneric(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(ctx.name(ref)) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs) <> ">"
           else
             Empty) <> "("
      ) <> Doc.args(args ++ outArgs) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    ) <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc =
    (if (outArgs.nonEmpty)
       Doc.fold(outArgs)(_ <> "," <+> _) <+> ":=" <+> Empty
     else
       Empty) <> Text(ctx.name(ref)) <> "(" <> Doc.args(args) <> ")"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutGeneric
    }

  def typeEnv: Map[Variable[G], Type[G]] = ref.decl.typeArgs.zip(typeArgs).toMap
}
