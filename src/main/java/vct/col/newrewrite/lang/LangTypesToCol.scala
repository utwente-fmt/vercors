package vct.col.newrewrite.lang

import vct.col.ast.{AxiomaticDataType, CBool, CChar, CDouble, CFloat, CLong, CPrimitiveType, CTypeSpecifier, CTypedefName, CVoid, JavaNamedType, JavaTClass, Model, PVLNamedType, Rewriter, TAxiomatic, TBool, TChar, TClass, TFloat, TInt, TModel, TNotAValue, TUnion, TVar, TVoid, Type}
import vct.col.resolve.{C, RefAxiomaticDataType, RefClass, RefJavaClass, RefModel, RefVariable, SpecTypeNameTarget}
import vct.result.VerificationResult.UserError

case class LangTypesToCol() extends Rewriter {
  case class CTypeNotSupported(t: CPrimitiveType) extends UserError {
    override def code: String = "cTypeNotSupported"
    override def text: String = t.o.messageInContext("This type is not supported by VerCors.")
  }

  override def dispatch(t: Type): Type = t match {
    case t @ JavaNamedType(_) =>
      t.ref.get match {
        case RefAxiomaticDataType(decl) => TAxiomatic(succ[AxiomaticDataType](decl), Nil)
        case RefModel(decl) => TModel(succ[Model](decl))
        case RefJavaClass(decl) => JavaTClass(succ(decl), Nil /* TODO */)
        case RefVariable(v) => TVar(v.ref)
      }
    case t @ PVLNamedType(_, typeArgs) =>
      t.ref.get match {
        case RefAxiomaticDataType(decl) => TAxiomatic(succ(decl), typeArgs.map(dispatch))
        case RefModel(decl) => TModel(succ(decl))
        case RefVariable(decl) => TVar(succ(decl))
        case RefClass(decl) => TClass(succ(decl))
      }
    case t @ CPrimitiveType(specs) =>
      specs.collect { case spec: CTypeSpecifier => spec } match {
        case Seq(CVoid()) => TVoid()
        case Seq(CChar()) => TChar()
        case t if C.NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
        case Seq(CFloat()) | Seq(CDouble()) | Seq(CLong(), CDouble()) => TFloat()
        case Seq(CBool()) => TBool()
        case Seq(defn @ CTypedefName(_)) => TNotAValue(defn.ref.get)
        case _ => throw CTypeNotSupported(t)
      }
    case other => rewriteDefault(other)
  }
}
