package vct.col.newrewrite.lang

import vct.col.ast.{AxiomaticDataType, CBool, CChar, CDeclaration, CDeclarationSpecifier, CDeclarator, CDouble, CFloat, CFunctionDefinition, CInit, CLong, CName, CParam, CPrimitiveType, CSpecificationType, CTypeSpecifier, CTypedFunctionDeclarator, CTypedefName, CVoid, Declaration, JavaNamedType, JavaTClass, Model, Node, PVLNamedType, TAxiomatic, TBool, TChar, TClass, TFloat, TInt, TModel, TNotAValue, TUnion, TVar, TVoid, Type}
import vct.col.origin.Origin
import vct.col.resolve.{C, RefAxiomaticDataType, RefClass, RefJavaClass, RefModel, RefVariable, SpecTypeNameTarget}
import vct.col.rewrite.Rewriter
import vct.col.ast.RewriteHelpers._
import vct.col.ref.{Ref, UnresolvedRef}
import vct.result.VerificationResult.UserError

import scala.reflect.ClassTag

case class LangTypesToCol() extends Rewriter {
  override def succ[T <: Declaration](ref: Ref[T])(implicit tag: ClassTag[T]): Ref[T] =
    ref match {
      // Retain unresolved references to be resolved by LangSpecificToCol
      case unresolved: UnresolvedRef[T] => unresolved
      case other =>
        succ(other.decl)
    }

  override def dispatch(t: Type): Type = {
    implicit val o: Origin = t.o
    t match {
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
        C.getPrimitiveType(specs, context = Some(t))
      case other => rewriteDefault(other)
    }
  }

  def normalizeCDeclaration(specifiers: Seq[CDeclarationSpecifier],
                            declarator: CDeclarator,
                            context: Option[Node] = None)
                           (implicit o: Origin): (Seq[CDeclarationSpecifier], CDeclarator) = {
    val info = C.getDeclaratorInfo(declarator)
    val baseType = C.getPrimitiveType(specifiers, context)
    val otherSpecifiers = specifiers.filter(!_.isInstanceOf[CTypeSpecifier]).map(dispatch)
    val newSpecifiers = CSpecificationType(info.typeOrReturnType(baseType)) +: otherSpecifiers
    val newDeclarator = info.params match {
      case Some(params) =>
        // PB TODO: varargs is discarded here.
        CTypedFunctionDeclarator(
          collectInScope(cParams) { params.foreach(dispatch) },
          varargs = false,
          CName(info.name)
        )
      case None =>
        CName(info.name)
    }

    (newSpecifiers, newDeclarator)
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case param: CParam =>
      val (specs, decl) = normalizeCDeclaration(param.specifiers, param.declarator, context = Some(param))(param.o)
      new CParam(specs, decl)(param.o).declareDefault(this)
    case declaration: CDeclaration =>
      declaration.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCDeclaration(declaration.specs, init.decl)
        declaration.rewrite(
          specs = specs,
          inits = Seq(
            CInit(decl, init.init.map(dispatch)),
          ),
        ).declareDefault(this)
      })
    case declaration: CFunctionDefinition =>
      implicit val o: Origin = declaration.o
      val (specs, decl) = normalizeCDeclaration(declaration.specs, declaration.declarator)
      declaration.rewrite(specs = specs, declarator = decl).declareDefault(this)
    case other => rewriteDefault(other)
  }
}
