package vct.col.ast

import vct.col.resolve.{PVLDerefTarget, PVLNameTarget, PVLTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}
import vct.result.VerificationResult

sealed trait PVLType extends ExtraType
case class PVLNamedType(name: String)(implicit val o: Origin = DiagnosticOrigin) extends PVLType {
  var ref: Option[PVLTypeNameTarget] = None

  override def mimics: Type = ref.get match {
    case RefAxiomaticDataType(decl) => TAxiomatic(decl.ref, Nil)
    case RefModel(decl) => TModel(decl.ref)
    case RefClass(decl) => TClass(decl.ref)
  }

  override protected def superTypeOfImpl(other: Type): Boolean =
    throw VerificationResult.Unreachable("PVL types always mimic a basic type.")
}

sealed trait PVLExpr extends ExtraExpr
case class PVLLocal(name: String)(implicit val o: Origin) extends PVLExpr with NoCheck {
  var ref: Option[PVLNameTarget] = None

  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case ref: RefVariable => ref.decl.t
    case ref: RefFunction => TNotAValue(ref)
    case ref: RefProcedure =>  TNotAValue(ref)
    case ref: RefPredicate => TNotAValue(ref)
    case ref: RefInstanceFunction => TNotAValue(ref)
    case ref: RefInstanceMethod => TNotAValue(ref)
    case ref: RefInstancePredicate => TNotAValue(ref)
    case ref: RefADTFunction => TNotAValue(ref)
    case ref: RefModelProcess => TNotAValue(ref)
    case ref: RefModelAction => TNotAValue(ref)
    case ref: RefField => ref.decl.t
  }
}

case class PVLDeref(obj: Expr, field: String)(implicit val o: Origin) extends PVLExpr with NoCheck {
  var ref: Option[PVLDerefTarget] = None

  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefFunction => TNotAValue(ref)
    case ref: RefProcedure => TNotAValue(ref)
    case ref: RefPredicate => TNotAValue(ref)
    case ref: RefInstanceFunction => TNotAValue(ref)
    case ref: RefInstanceMethod => TNotAValue(ref)
    case ref: RefInstancePredicate => TNotAValue(ref)
    case ref: RefADTFunction => TNotAValue(ref)
    case ref: RefModelProcess => TNotAValue(ref)
    case ref: RefModelAction => TNotAValue(ref)
    case ref: RefField => ref.decl.t
  }
}