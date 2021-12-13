package vct.col.ast.temporaryimplpackage.`type`.typeclass

import vct.col.ast._
import vct.col.check.{CheckContext, CheckError}
import vct.col.coerce.Coercion
import vct.col.ref.Ref
import vct.col.rewrite.{NonLatchingRewriter, Rewriter}

import scala.reflect.ClassTag

trait TypeImpl[G] { this: Type[G] =>
  def superTypeOf(other: Type[G]): Boolean =
    Coercion.getCoercion(other, this).isDefined

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil

  def asSeq: Option[TSeq[G]] = Coercion.getAnySeqCoercion(this).map(_._2)
  def asSet: Option[TSet[G]] = Coercion.getAnySetCoercion(this).map(_._2)
  def asBag: Option[TBag[G]] = Coercion.getAnyBagCoercion(this).map(_._2)
  def asPointer: Option[TPointer[G]] = Coercion.getAnyPointerCoercion(this).map(_._2)
  def asArray: Option[TArray[G]] = Coercion.getAnyArrayCoercion(this).map(_._2)
  def asOption: Option[TOption[G]] = Coercion.getAnyOptionCoercion(this).map(_._2)
  def asMap: Option[TMap[G]] = Coercion.getAnyMapCoercion(this).map(_._2)
  def asTuple: Option[TTuple[G]] = Coercion.getAnyTupleCoercion(this).map(_._2)
  def asMatrix: Option[TMatrix[G]] = Coercion.getAnyMatrixCoercion(this).map(_._2)
  def asModel: Option[TModel[G]] = Coercion.getAnyModelCoercion(this).map(_._2)
  def asClass: Option[TClass[G]] = Coercion.getAnyClassCoercion(this).map(_._2)
  def asEither: Option[TEither[G]] = Coercion.getAnyEitherCoercion(this).map(_._2)
  /*def asVector: Option[TVector] = optMatch(this) { case vec: TVector => vec }*/

  def particularize(substitutions: Map[Variable[G], Type[G]]): Type[G] = {
    case object Particularize extends NonLatchingRewriter[G, G] {
      override def succ[DPre <: Declaration[G], DPost <: Declaration[G]](decl: DPre)(implicit tag: ClassTag[DPost]): Ref[G, DPost] =
        decl.asInstanceOf[DPost].ref

      override def dispatch(t: Type[G]): Type[G] = t match {
        case TVar(Ref(v)) => substitutions(v)
        case other => rewriteDefault(other)
      }
    }
    Particularize.dispatch(this)
  }
}
