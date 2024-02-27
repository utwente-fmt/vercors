package vct.col.ast.`type`

import vct.col.ast.{Applicable, Class, ClassDeclaration, Constructor, ContractApplicable, InstanceField, InstanceFunction, InstanceMethod, InstanceOperatorFunction, InstanceOperatorMethod, TClass, Type, Variable}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.ast.ops.TClassOps
import vct.col.ref.Ref
import vct.result.VerificationError.Unreachable

trait TClassImpl[G] extends TClassOps[G] { this: TClass[G] =>
  def transSupportArrows: Seq[(Class[G], Class[G])] = cls.decl.transSupportArrows

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(cls)) <> (
    if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty)

  def typeEnv: Map[Variable[G], Type[G]] = cls.decl.typeArgs.zip(typeArgs).toMap

  def instantiate(t: Type[G]): Type[G] =
    this match {
      case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
        t.particularize(cls.typeArgs.zip(typeArgs).toMap)
      case _ => t
    }

  def fieldType(decl: InstanceField[G]): Type[G] = instantiate(decl.t)
}