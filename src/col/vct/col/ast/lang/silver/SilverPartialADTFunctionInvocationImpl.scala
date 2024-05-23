package vct.col.ast.lang.silver

import vct.col.ast.{
  ADTFunction,
  AxiomaticDataType,
  SilverPartialADTFunctionInvocation,
  Type,
}
import vct.col.print._
import vct.col.ref.Ref
import vct.col.ast.ops.SilverPartialADTFunctionInvocationOps

trait SilverPartialADTFunctionInvocationImpl[G]
    extends SilverPartialADTFunctionInvocationOps[G] {
  this: SilverPartialADTFunctionInvocation[G] =>
  def adt: AxiomaticDataType[G] = ref.get._1
  def function: ADTFunction[G] = ref.get._2

  def maybeTypeArgs: Option[Seq[Type[G]]] =
    Some(adt.typeArgs.map(arg =>
      partialTypeArgs.collectFirst { case (Ref(v), t) if arg == v => t }
        .getOrElse(return None)
    ))

  def typeArgs: Seq[Type[G]] = maybeTypeArgs.get

  override lazy val t: Type[G] = function.returnType
    .particularize(adt.typeArgs.zip(typeArgs).toMap)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(name) <> "(" <> Doc.args(args) <> ")")
}
