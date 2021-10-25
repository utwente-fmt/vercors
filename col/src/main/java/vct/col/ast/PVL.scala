package vct.col.ast

import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, PVLDerefTarget, PVLInvocationTarget, PVLNameTarget, PVLTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}
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
case class PVLLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr with NoCheck {
  var ref: Option[PVLNameTarget] = None

  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case ref: RefVariable => ref.decl.t
    case ref: RefClass => TNotAValue(ref)
    case ref: RefField => ref.decl.t
    case ref: RefModelField => ref.decl.t
  }
}

case class PVLDeref(obj: Expr, field: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr with NoCheck {
  var ref: Option[PVLDerefTarget] = None

  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefField => ref.decl.t
    case ref: BuiltinField => ref.f(obj).t
  }
}

case class PVLInvocation(obj: Option[Expr], method: String, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])
                        (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends PVLExpr with NoCheck {
  var ref: Option[PVLInvocationTarget] = None

  override def t: Type = ref.get match {
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(_) => TResource()
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(_) => TResource()
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(_) => TProcess()
    case RefModelAction(_) => TProcess()
    case BuiltinInstanceMethod(f) => f(obj.get)(args).t
  }
}

case class PVLNew(t: Type, args: Seq[Expr])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends PVLExpr with NoCheck

sealed trait PVLClassDeclaration extends ExtraClassDeclaration
class PVLConstructor(val contract: ApplicableContract, val args: Seq[Variable], val body: Option[Statement])(implicit val o: Origin)
  extends PVLClassDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = args ++ contract.givenArgs ++ contract.yieldsArgs
}