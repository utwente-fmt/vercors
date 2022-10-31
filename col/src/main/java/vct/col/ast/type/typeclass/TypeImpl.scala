package vct.col.ast.`type`.typeclass

import vct.col.ast._
import vct.col.check.{CheckContext, CheckError}
import vct.col.ref.Ref
import vct.col.rewrite.{NonLatchingRewriter, Rewriter}
import vct.col.typerules.CoercionUtils

import scala.reflect.ClassTag

trait TypeImpl[G] { this: Type[G] =>
  def superTypeOf(other: Type[G]): Boolean =
    CoercionUtils.getCoercion(other, this).isDefined

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil

  def asSeq: Option[TSeq[G]] = CoercionUtils.getAnySeqCoercion(this).map(_._2)
  def asSet: Option[TSet[G]] = CoercionUtils.getAnySetCoercion(this).map(_._2)
  def asBag: Option[TBag[G]] = CoercionUtils.getAnyBagCoercion(this).map(_._2)
  def asPointer: Option[TPointer[G]] = CoercionUtils.getAnyPointerCoercion(this).map(_._2)
  def asArray: Option[TArray[G]] = CoercionUtils.getAnyArrayCoercion(this).map(_._2)
  def asOption: Option[TOption[G]] = CoercionUtils.getAnyOptionCoercion(this).map(_._2)
  def asMap: Option[TMap[G]] = CoercionUtils.getAnyMapCoercion(this).map(_._2)
  def asTuple: Option[TTuple[G]] = CoercionUtils.getAnyTupleCoercion(this).map(_._2)
  def asMatrix: Option[TMatrix[G]] = CoercionUtils.getAnyMatrixCoercion(this).map(_._2)
  def asModel: Option[TModel[G]] = CoercionUtils.getAnyModelCoercion(this).map(_._2)
  def asClass: Option[TClass[G]] = CoercionUtils.getAnyClassCoercion(this).map(_._2)
  def asEither: Option[TEither[G]] = CoercionUtils.getAnyEitherCoercion(this).map(_._2)
  /*def asVector: Option[TVector] = optMatch(this) { case vec: TVector => vec }*/

  def particularize(substitutions: Map[Variable[G], Type[G]]): Type[G] = {
    case object Particularize extends NonLatchingRewriter[G, G] {
      case object IdentitySuccessorsProvider extends SuccessorsProviderTrafo[G, G](allScopes.freeze) {
        override def preTransform[I <: Declaration[G], O <: Declaration[G]](pre: I): Option[O] =
          Some(pre.asInstanceOf[O])
      }

      override def succProvider: SuccessorsProvider[G, G] = IdentitySuccessorsProvider

      override def dispatch(t: Type[G]): Type[G] = t match {
        case TVar(Ref(v)) => substitutions(v)
        case other => rewriteDefault(other)
      }
    }
    Particularize.dispatch(this)
  }
}
