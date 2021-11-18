package vct.col.ast

import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, PVLDerefTarget, PVLInvocationTarget, PVLNameTarget, PVLTypeNameTarget, RefADTFunction, RefAxiomaticDataType, RefClass, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget, SpecTypeNameTarget}
import vct.col.origin._
import vct.result.VerificationResult

sealed trait PVLType extends ExtraType
case class PVLNamedType(name: String, typeArgs: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends PVLType {
  var ref: Option[PVLTypeNameTarget] = None
}

sealed trait PVLExpr extends ExtraExpr
case class PVLLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr {
  var ref: Option[PVLNameTarget] = None

  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => TNotAValue(ref)
    case ref: RefVariable => ref.decl.t
    case ref: RefClass => TNotAValue(ref)
    case ref: RefField => ref.decl.t
    case ref: RefModelField => ref.decl.t
  }
}

case class PVLDeref(obj: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends PVLExpr {
  var ref: Option[PVLDerefTarget] = None

  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefField => ref.decl.t
    case ref: BuiltinField => ref.f(obj).t
  }
}

case class PVLInvocation(obj: Option[Expr], method: String, args: Seq[Expr], typeArgs: Seq[Type],
                         givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])
                        (val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends PVLExpr {
  var ref: Option[PVLInvocationTarget] = None

  override def t: Type = ref.get match {
    case RefFunction(decl) => decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(_) => TResource()
    case RefInstanceFunction(decl) => decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(_) => TResource()
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(_) => TProcess()
    case RefModelAction(_) => TProcess()
    case BuiltinInstanceMethod(f) => f(obj.get)(args).t
  }
}

case class PVLNew(t: Type, args: Seq[Expr])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends PVLExpr

sealed trait PVLClassDeclaration extends ExtraClassDeclaration
class PVLConstructor(val contract: ApplicableContract, val args: Seq[Variable], val body: Option[Statement])(implicit val o: Origin)
  extends PVLClassDeclaration with Declarator {
  override def declarations: Seq[Declaration] = args ++ contract.givenArgs ++ contract.yieldsArgs
}