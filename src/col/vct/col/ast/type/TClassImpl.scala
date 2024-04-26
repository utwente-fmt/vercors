package vct.col.ast.`type`

import vct.col.ast.{Applicable, Class, ClassDeclaration, Constructor, ContractApplicable, InstanceField, InstanceFunction, InstanceMethod, InstanceOperatorFunction, InstanceOperatorMethod, TClass, Type, Variable}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.ast.ops.TClassOps
import vct.col.ref.Ref
import vct.result.VerificationError.Unreachable

trait TClassImpl[G] extends TClassOps[G] { this: TClass[G] =>
  def transSupportArrows: Seq[(TClass[G], TClass[G])] = cls.decl.transSupportArrows.map {
    // TODO (RR): To implement this, cls.supports needs to be TClass instead of Class
    case (clsA, clsB) => (TClass(clsA.ref, Seq()), TClass(clsB.ref, Seq()))
  }

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