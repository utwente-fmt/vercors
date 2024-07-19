package vct.rewrite

import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.col.typerules.CoercingRewriter
import vct.result.VerificationError.UserError

case class DisallowedConstAssignment(target: Node[_]) extends UserError {
  override def code: String = "disallowedConstAssignment"
  override def text: String =
    target.o.messageInContext("Cannot assign to constant target.")
}

case class DisallowedQualifiedType(target: Node[_]) extends UserError {
  override def code: String = "disallowedQualifiedType"
  override def text: String =
    target.o.messageInContext("This qualified type is not allowed.")
}

case object TypeQualifierCoercion extends RewriterBuilder {
  override def key: String = "TypeQualifierCoercion"
  override def desc: String =
    "Removes qualifiers from types."
}

case class TypeQualifierCoercion[Pre <: Generation]()
    extends CoercingRewriter[Pre] {

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
    implicit o: Origin
  ): Expr[Post] = {
    coercion match {
      case CoerceFromConst(_) =>
      case CoerceToConst(_) =>
      case CoerceToUnique(_, _) =>
      case CoerceFromUnique(_, _) =>
      case CoerceBetweenUnique(_, _, _) =>
      case _ =>
    }
    super.applyCoercion(e, coercion)
  }

  case class InnerInfo(){
    var unique: Option[BigInt] = None
    var const: Boolean = false
  }

  def getUnqualified(t: Type[Pre], info: InnerInfo = InnerInfo()): (InnerInfo, Type[Post]) = t match {
    case TConst(_) | TUnique(_, _) if info.const || info.unique.isDefined =>
      throw DisallowedQualifiedType(t)
    case TConst(it) =>
      info.const = true
      getUnqualified(it, info)
    case TUnique(it, id) =>
      info.unique = Some(id)
      getUnqualified(it, info)
    case _ => (info, dispatch(t))
  }

  def makePointer(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    val (info, resType) = getUnqualified(t)
    if(info.const) TConstPointer(resType)
    else if (info.unique.isDefined) TUniquePointer(resType, info.unique.get)
    else TPointer(resType)
  }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TConst(t) => dispatch(t)
      case TUnique(_, _) => throw DisallowedQualifiedType(t)
      case TPointer(it) => makePointer(it)
      case other => other.rewriteDefault()
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case PreAssignExpression(target, _) if target.t.isInstanceOf[TConst[Pre]] => throw DisallowedConstAssignment(target)
      case PostAssignExpression(target, _) if target.t.isInstanceOf[TConst[Pre]] => throw DisallowedConstAssignment(target)
      case npa @ NewPointerArray(t, size, _) =>
        val (info, newT) = getUnqualified(t)
        if(info.const) NewConstPointerArray(newT, dispatch(size))(npa.blame)
        else NewPointerArray(newT, dispatch(size), info.unique)(npa.blame)
      case other => other.rewriteDefault()
    }
  }

  override def postCoerce(s: Statement[Pre]): Statement[Post] =
    s match {
      case Assign(target, _) if getUnqualified(target.t)._1.const => throw DisallowedConstAssignment(target)
      case a@AssignInitial(target, value) => Assign(dispatch(target), dispatch(value))(a.blame)(a.o)
      case other => other.rewriteDefault()
    }
}
