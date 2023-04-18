package vct.col.ast.expr.apply

import vct.col.ast.{ADTFunction, ADTFunctionInvocation, Type}
import vct.col.print._
import vct.col.ref.Ref

trait ADTFunctionInvocationImpl[G] { this: ADTFunctionInvocation[G] =>
  override def ref: Ref[G, _ <: ADTFunction[G]]
  override lazy val t: Type[G] =
    typeArgs match {
      case Some((adt, typeArgs)) =>
        ref.decl.returnType.particularize(adt.decl.typeArgs.zip(typeArgs).toMap)
      case None => ref.decl.returnType
    }

  def layoutSpec(implicit ctx: Ctx): Doc = typeArgs match {
    case None => layoutSilver // will not be resolvable anyway
    case Some((adt, typeArgs)) =>
      Group(
        Group(
          Text(ctx.name(adt)) <>
          (if(typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty) <>
          "." <> ctx.name(ref) <> "("
        ) <> Doc.args(args) <> ")"
      )
  }

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text(ctx.name(ref)) <> "(" <> Doc.args(args) <> ")")

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => layoutSpec
  }
}