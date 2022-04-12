package vct.col.newrewrite.lang

import vct.col.ast.{AxiomaticDataType, CBool, CChar, CDeclaration, CDeclarationSpecifier, CDeclarator, CDouble, CFloat, CFunctionDefinition, CInit, CLong, CName, CParam, CPrimitiveType, CSpecificationType, CTypeSpecifier, CTypedFunctionDeclarator, CTypedefName, CVoid, Declaration, JavaClass, JavaNamedType, JavaTClass, Model, Node, PVLNamedType, SilverPartialTAxiomatic, PinnedDecl, TAxiomatic, TBool, TChar, TClass, TFloat, TInt, TModel, TNotAValue, TUnion, TVar, TVoid, Type}
import vct.col.origin.Origin
import vct.col.resolve.{C, RefAxiomaticDataType, RefClass, RefJavaClass, RefModel, RefVariable, SpecTypeNameTarget}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast.RewriteHelpers._
import vct.col.newrewrite.lang.LangTypesToCol.IncompleteTypeArgs
import vct.col.ref.{Ref, UnresolvedRef}
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
  override def succ[DPost <: Declaration[Rewritten[Pre]]](ref: Ref[Pre, _ <: Declaration[Pre]])(implicit tag: ClassTag[DPost]): Ref[Rewritten[Pre], DPost] =
    ref match {
      // Retain unresolved references to be resolved by LangSpecificToCol
      case unresolved: UnresolvedRef[_, _] => new UnresolvedRef[Post, DPost](unresolved.name)
      case other => succ(other.decl)
    }

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case t @ JavaNamedType(_) =>
        t.ref.get match {
          case RefAxiomaticDataType(decl) => TAxiomatic[Post](succ(decl), Nil)
          case RefModel(decl) => TModel[Post](succ(decl))
          case RefJavaClass(decl) =>
            assert(t.names.init.map(_._2).forall((x: Option[Seq[Type[Pre]]]) => x.isEmpty))
            val x = JavaTClass[Post](succ(decl), t.names.last._2.getOrElse(Nil).map(dispatch))
            x
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
    case cls: JavaClass[Pre] =>
      rewriteDefault(cls)
      succ[JavaClass[Post]](cls).decl.pin = cls.pin.map(dispatch(_))
    case other => rewriteDefault(other)
  }
}
