package vct.col.ast.temporaryimplpackage.`type`.typeclass

import vct.col.ast._
import vct.col.check.{CheckContext, CheckError}
import vct.col.coerce.Coercion
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter

trait TypeImpl { this: Type =>
  def superTypeOf(other: Type): Boolean =
    Coercion.getCoercion(other, this).isDefined

  override def check(context: CheckContext): Seq[CheckError] = Nil

  def asSeq: Option[TSeq] = Coercion.getAnySeqCoercion(this).map(_._2)
  def asSet: Option[TSet] = Coercion.getAnySetCoercion(this).map(_._2)
  def asBag: Option[TBag] = Coercion.getAnyBagCoercion(this).map(_._2)
  def asPointer: Option[TPointer] = Coercion.getAnyPointerCoercion(this).map(_._2)
  def asArray: Option[TArray] = Coercion.getAnyArrayCoercion(this).map(_._2)
  def asOption: Option[TOption] = Coercion.getAnyOptionCoercion(this).map(_._2)
  def asMap: Option[TMap] = Coercion.getAnyMapCoercion(this).map(_._2)
  def asTuple: Option[TTuple] = Coercion.getAnyTupleCoercion(this).map(_._2)
  def asMatrix: Option[TMatrix] = Coercion.getAnyMatrixCoercion(this).map(_._2)
  def asModel: Option[TModel] = Coercion.getAnyModelCoercion(this).map(_._2)
  def asEither: Option[TEither] = Coercion.getAnyEitherCoercion(this).map(_._2)
  /*def asVector: Option[TVector] = optMatch(this) { case vec: TVector => vec }*/

  def particularize(substitutions: Map[Variable, Type]): Type = {
    case object Particularize extends Rewriter {
      override def dispatch(t: Type): Type = t match {
        case TVar(Ref(v)) => substitutions(v)
        case _ => t match {
          case JavaTClass(ref, args) => JavaTClass(ref, args)
          case TModel(ref) => TModel(ref)
          case TClass(ref) => TClass(ref)
          case TAxiomatic(ref, args) => TAxiomatic(ref, args.map(dispatch))
          case other => rewriteDefault(other)
        }
      }
    }
    Particularize.dispatch(this)
  }
}
