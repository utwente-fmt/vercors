package vct.col.ast.lang

import vct.col.ast.{ADTFunction, AxiomaticDataType, SilverPartialADTFunctionInvocation, TAny, Type}
import vct.col.ref.Ref

trait SilverPartialADTFunctionInvocationImpl[G] { this: SilverPartialADTFunctionInvocation[G] =>
  def adt: AxiomaticDataType[G] = ref.get._1
  def function: ADTFunction[G] = ref.get._2

  def maybeTypeArgs: Option[Seq[Type[G]]] =
    Some(adt.typeArgs.map(arg => partialTypeArgs.collectFirst { case (Ref(v), t) if arg == v => t }.getOrElse(return None)))

  def typeArgs: Seq[Type[G]] = maybeTypeArgs.get

  override lazy val t: Type[G] = function.returnType.particularize(adt.typeArgs.zip(typeArgs).toMap)
}
