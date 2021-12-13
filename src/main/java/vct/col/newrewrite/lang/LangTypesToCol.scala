package vct.col.newrewrite.lang

import vct.col.ast.{AxiomaticDataType, CBool, CChar, CDeclaration, CDeclarationSpecifier, CDeclarator, CDouble, CFloat, CFunctionDefinition, CInit, CLong, CName, CParam, CPrimitiveType, CSpecificationType, CTypeSpecifier, CTypedFunctionDeclarator, CTypedefName, CVoid, Declaration, JavaNamedType, JavaTClass, Model, Node, PVLNamedType, TAxiomatic, TBool, TChar, TClass, TFloat, TInt, TModel, TNotAValue, TUnion, TVar, TVoid, Type}
import vct.col.origin.Origin
import vct.col.resolve.{C, RefAxiomaticDataType, RefClass, RefJavaClass, RefModel, RefVariable, SpecTypeNameTarget}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast.RewriteHelpers._
import vct.col.ref.{Ref, UnresolvedRef}
import vct.result.VerificationResult.UserError

import scala.reflect.ClassTag

case object LangTypesToCol extends RewriterBuilder

case class LangTypesToCol[Pre <: Generation]() extends Rewriter[Pre] {
  override def succ[DPre <: Declaration[Pre], DPost <: Declaration[Rewritten[Pre]]](ref: Ref[Pre, DPre])(implicit tag: ClassTag[DPost]): Ref[Rewritten[Pre], DPost] =
    ref match {
      // Retain unresolved references to be resolved by LangSpecificToCol
      case unresolved: UnresolvedRef[Pre, DPre] => new UnresolvedRef[Post, DPost](unresolved.name)
      case other => succ(other.decl)
    }

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case t @ JavaNamedType(_) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => TAxiomatic[Post](succ(decl), Nil)
          case RefModel(decl) => TModel[Post](succ(decl))
          case RefJavaClass(decl) => JavaTClass[Post](succ(decl), Nil /* TODO */)
          case RefVariable(v) => TVar[Post](succ(v))
        }
      case t @ PVLNamedType(_, typeArgs) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => TAxiomatic(succ(decl), typeArgs.map(dispatch))
          case RefModel(decl) => TModel(succ(decl))
          case RefVariable(decl) => TVar(succ(decl))
          case RefClass(decl) => TClass(succ(decl))
        }
      case t @ CPrimitiveType(specs) =>
        dispatch(C.getPrimitiveType(specs, context = Some(t)))
      case other => rewriteDefault(other)
    }
  }

  def normalizeCDeclaration(specifiers: Seq[CDeclarationSpecifier[Pre]],
                            declarator: CDeclarator[Pre],
                            context: Option[Node[Pre]] = None)
                           (implicit o: Origin): (Seq[CDeclarationSpecifier[Post]], CDeclarator[Post]) = {
    val info = C.getDeclaratorInfo(declarator)
    val baseType = C.getPrimitiveType(specifiers, context)
    val otherSpecifiers = specifiers.filter(!_.isInstanceOf[CTypeSpecifier[Pre]]).map(dispatch)
    val newSpecifiers = CSpecificationType[Post](dispatch(info.typeOrReturnType(baseType))) +: otherSpecifiers
    val newDeclarator = info.params match {
      case Some(params) =>
        // PB TODO: varargs is discarded here.
        CTypedFunctionDeclarator[Post](
          collectInScope(cParams) { params.foreach(dispatch) },
          varargs = false,
          CName(info.name)
        )
      case None =>
        CName[Post](info.name)
    }

    (newSpecifiers, newDeclarator)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case param: CParam[Pre] =>
      val (specs, decl) = normalizeCDeclaration(param.specifiers, param.declarator, context = Some(param))(param.o)
      new CParam(specs, decl)(param.o).declareDefault(this)
    case declaration: CDeclaration[Pre] =>
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
    case declaration: CFunctionDefinition[Pre] =>
      implicit val o: Origin = declaration.o
      val (specs, decl) = normalizeCDeclaration(declaration.specs, declaration.declarator)
      declaration.rewrite(specs = specs, declarator = decl).declareDefault(this)
    case other => rewriteDefault(other)
  }
}
