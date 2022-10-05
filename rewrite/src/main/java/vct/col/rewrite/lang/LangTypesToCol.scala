package vct.col.rewrite.lang

import vct.col.ast.{AxiomaticDataType, CArrayDeclarator, CBool, CChar, CDeclaration, CDeclarationSpecifier, CDeclarator, CFunctionDefinition, CGlobalDeclaration, CInit, CLocalDeclaration, CLong, CName, CParam, CPrimitiveType, CSpecificationType, CTypeSpecifier, CTypedFunctionDeclarator, CTypedefName, CVoid, Declaration, JavaEnum, JavaNamedType, JavaTClass, Model, Node, PVLNamedType, SilverPartialTAxiomatic, TAxiomatic, TBool, TChar, TClass, TEnum, TFloat, TInt, TModel, TNotAValue, TUnion, TVar, TVoid, Type}
import vct.col.origin.Origin
import vct.col.resolve.ctx.{RefAxiomaticDataType, RefClass, RefEnum, RefJavaClass, RefModel, RefVariable, SpecTypeNameTarget}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.lang.LangTypesToCol.IncompleteTypeArgs
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.lang.C
import vct.result.VerificationError.UserError

import scala.reflect.ClassTag

case object LangTypesToCol extends RewriterBuilder {
  override def key: String = "langTypes"
  override def desc: String = "Translate language-specific types (such as named types) to specific internal types."

  case class IncompleteTypeArgs(t: SilverPartialTAxiomatic[_]) extends UserError {
    override def code: String = "incompleteTypeArgs"
    override def text: String =
      t.o.messageInContext("This type does not specify all generic types for the domain.")
  }
}

case class LangTypesToCol[Pre <: Generation]() extends Rewriter[Pre] {
  override def porcelainRefSucc[RefDecl <: Declaration[Rewritten[Pre]]](ref: Ref[Pre, _])(implicit tag: ClassTag[RefDecl]): Option[Ref[Rewritten[Pre], RefDecl]] =
    ref match {
      // Retain unresolved references to be resolved by LangSpecificToCol
      case unresolved: UnresolvedRef[_, _] => Some(new UnresolvedRef[Post, RefDecl](unresolved.name))
      case _ => None
    }

  override def porcelainRefSeqSucc[RefDecl <: Declaration[Rewritten[Pre]]](refs: Seq[Ref[Pre, _]])(implicit tag: ClassTag[RefDecl]): Option[Seq[Ref[Rewritten[Pre], RefDecl]]] =
    if(refs.forall(_.isInstanceOf[UnresolvedRef[_, _]]))
      Some(refs.map(porcelainRefSucc[RefDecl]).map(_.get))
    else None

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case t @ JavaNamedType(_) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => TAxiomatic[Post](succ(decl), Nil)
          case RefModel(decl) => TModel[Post](succ(decl))
          case RefJavaClass(decl) => JavaTClass[Post](succ(decl), Nil /* TODO */)
          case RefVariable(v) => TVar[Post](succ(v))
          case RefEnum(enum) => TEnum[Post](succ(enum))
        }
      case t @ PVLNamedType(_, typeArgs) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => TAxiomatic(succ(decl), typeArgs.map(dispatch))
          case RefModel(decl) => TModel(succ(decl))
          case RefVariable(decl) => TVar(succ(decl))
          case RefClass(decl) => TClass(succ(decl))
          case RefEnum(decl) => TEnum(succ(decl))
        }
      case t @ CPrimitiveType(specs) =>
        dispatch(C.getPrimitiveType(specs, context = Some(t)))
      case t @ SilverPartialTAxiomatic(Ref(adt), partialTypeArgs) =>
        if(partialTypeArgs.map(_._1.decl).toSet != adt.typeArgs.toSet)
          throw IncompleteTypeArgs(t)

        TAxiomatic(succ(adt), adt.typeArgs.map(arg => dispatch(t.partialTypeArgs.find(_._1.decl == arg).get._2)))
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
          cParams.dispatch(params),
          varargs = false,
          CName(info.name)
        )
      case None =>
        CName[Post](info.name)
    }
    declarator match {
      case CArrayDeclarator(_, Some(size), _) =>
        val spec = CSpecificationType[Post](dispatch(baseType)) +: otherSpecifiers
        return (spec, CArrayDeclarator(Seq(), Some(dispatch(size)), newDeclarator))
      case _ =>
    }

    (newSpecifiers, newDeclarator)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case param: CParam[Pre] =>
      val (specs, decl) = normalizeCDeclaration(param.specifiers, param.declarator, context = Some(param))(param.o)
      cParams.declare(new CParam(specs, decl)(param.o))
    case declaration: CLocalDeclaration[Pre] =>
      declaration.decl.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCDeclaration(declaration.decl.specs, init.decl)
        cLocalDeclarations.declare(declaration.rewrite(
          decl = declaration.decl.rewrite(
            specs = specs,
            inits = Seq(
              CInit(decl, init.init.map(dispatch)),
            ),
          ),
        ))
      })
    case declaration: CGlobalDeclaration[Pre] =>
      declaration.decl.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCDeclaration(declaration.decl.specs, init.decl)
        globalDeclarations.declare(declaration.rewrite(
          decl = declaration.decl.rewrite(
            specs = specs,
            inits = Seq(
              CInit(decl, init.init.map(dispatch)),
            ),
          ),
        ))
      })
    case declaration: CFunctionDefinition[Pre] =>
      implicit val o: Origin = declaration.o
      val (specs, decl) = normalizeCDeclaration(declaration.specs, declaration.declarator)
      globalDeclarations.declare(declaration.rewrite(specs = specs, declarator = decl))
    case other => rewriteDefault(other)
  }
}
