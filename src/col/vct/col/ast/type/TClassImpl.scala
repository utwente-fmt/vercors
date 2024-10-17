package vct.col.ast.`type`

import vct.col.ast.{
  Class,
  InstanceField,
  TByReferenceClass,
  TByValueClass,
  TClass,
  Type,
  Variable,
}
import vct.col.print._
import vct.col.ref.Ref

trait TClassImpl[G] {
  this: TClass[G] =>
  def cls: Ref[G, Class[G]]

  def typeArgs: Seq[Type[G]]

  def transSupportArrowsHelper(
      seen: Set[TClass[G]]
  ): Seq[(TClass[G], TClass[G])] =
    cls.decl.transSupportArrowsHelper(seen).map { case (clsA, clsB) =>
      (instantiate(clsA).asClass.get, instantiate(clsB).asClass.get)
    }

  def transSupportArrows(): Seq[(TClass[G], TClass[G])] =
    transSupportArrowsHelper(Set.empty)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(ctx.name(cls)) <>
        (if (typeArgs.nonEmpty)
           Text("<") <> Doc.args(typeArgs) <> ">"
         else
           Empty)
    )

  def typeEnv: Map[Variable[G], Type[G]] = cls.decl.typeArgs.zip(typeArgs).toMap

  def instantiate(t: Type[G]): Type[G] =
    this match {
      case TByReferenceClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
        t.particularize(cls.typeArgs.zip(typeArgs).toMap)
      case TByValueClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
        t.particularize(cls.typeArgs.zip(typeArgs).toMap)
      case _ => t
    }

  def fieldType(decl: InstanceField[G]): Type[G] = instantiate(decl.t)
}
