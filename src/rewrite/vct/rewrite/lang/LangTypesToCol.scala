package vct.col.rewrite.lang

import vct.col.ast.RewriteHelpers._
import vct.col.ast.{TModel, _}
import vct.col.origin.Origin
import vct.col.ref.{Ref, UnresolvedRef}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.{C, CPP}
import vct.col.rewrite.lang.LangTypesToCol.IncompleteTypeArgs
import vct.col.typerules.Types
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
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
      case unresolved: UnresolvedRef[_, _] if !unresolved.isResolved => Some(new UnresolvedRef[Post, RefDecl](unresolved.name))
      case _ => None
    }

  override def porcelainRefSeqSucc[RefDecl <: Declaration[Rewritten[Pre]]](refs: Seq[Ref[Pre, _]])(implicit tag: ClassTag[RefDecl]): Option[Seq[Ref[Rewritten[Pre], RefDecl]]] =
    if(refs.forall(_.isInstanceOf[UnresolvedRef[_, _]]))
      Some(refs.map(porcelainRefSucc[RefDecl]).map(_.get))
    else None

  def specType(target: SpecTypeNameTarget[Pre], args: Seq[Type[Pre]]): Type[Post] = target match {
    case RefAxiomaticDataType(decl) => TAxiomatic[Post](succ(decl), args.map(dispatch))
    case RefModel(decl) => TModel[Post](succ(decl))
    case RefEnum(enum) => TEnum[Post](succ(enum))
    case RefProverType(typ) => TProverType[Post](succ(typ))
    case RefVariable(decl) => TVar[Post](succ(decl))
  }

  override def dispatch(t: Type[Pre]): Type[Post] = {
    implicit val o: Origin = t.o
    t match {
      case t @ JavaNamedType(_) =>
        t.ref.get match {
          case spec: SpecTypeNameTarget[Pre] => specType(spec, Nil)
          case RefJavaClass(decl) =>
            assert(t.names.init.map(_._2).forall((x: Option[Seq[Type[Pre]]]) => x.isEmpty))
            val x = JavaTClass[Post](succ(decl), t.names.last._2.getOrElse(Nil).map(dispatch))
            x
        }
      case t @ PVLNamedType(_, typeArgs) =>
        t.ref.get match {
          case spec: SpecTypeNameTarget[Pre] => specType(spec, typeArgs)
          case RefClass(decl) => TClass(succ(decl))
        }
      case t @ CPrimitiveType(specs) =>
        dispatch(C.getPrimitiveType(specs, context = Some(t)))
      case t@CPPPrimitiveType(specs) =>
        dispatch(CPP.getBaseTypeFromSpecs(specs, context = Some(t)))
      case t @ SilverPartialTAxiomatic(Ref(adt), partialTypeArgs) =>
        if(partialTypeArgs.map(_._1.decl).toSet != adt.typeArgs.toSet)
          throw IncompleteTypeArgs(t)

        TAxiomatic(succ(adt), adt.typeArgs.map(arg => dispatch(t.partialTypeArgs.find(_._1.decl == arg).get._2)))
      case TNotAValue(decl) =>
        decl match {
          case RefCStruct(decl) =>
            CTStruct(succ(decl))
          case RefAxiomaticDataType(decl) => TAxiomatic[Post](succ(decl), Nil)
          case _ => rewriteDefault(t)
        }
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

    (newSpecifiers, newDeclarator)
  }

  def normalizeCPPDeclaration(specifiers: Seq[CPPDeclarationSpecifier[Pre]],
                            declarator: CPPDeclarator[Pre],
                            context: Option[Node[Pre]] = None)
                           (implicit o: Origin): (Seq[CPPDeclarationSpecifier[Post]], CPPDeclarator[Post]) = {
    val info = CPP.getDeclaratorInfo(declarator, context.getOrElse(false).isInstanceOf[CPPParam[Pre]])
    val baseType = CPP.getBaseTypeFromSpecs(specifiers, context)
    val otherSpecifiers = specifiers.filter(!_.isInstanceOf[CPPTypeSpecifier[Pre]]).map(dispatch)
    val newSpecifiers = CPPSpecificationType[Post](dispatch(info.typeOrReturnType(baseType))) +: otherSpecifiers
    val newDeclarator = info.params match {
      case Some(params) =>
        // PB TODO: varargs is discarded here.
        CPPTypedFunctionDeclarator[Post](
          cPPParams.dispatch(params),
          varargs = false,
          CPPName(info.name)
        )
      case None =>
        CPPName[Post](info.name)
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
        val (specs, decl) = normalizeCDeclaration(declaration.decl.specs, init.decl, context = Some(declaration))
        cLocalDeclarations.declare(declaration.rewrite(
          decl = declaration.decl.rewrite(
            specs = specs,
            inits = Seq(
              CInit(decl, init.init.map(dispatch)),
            ),
          ),
        ))
      })
    case declaration@CGlobalDeclaration(CDeclaration(_, _, Seq(_: CStructDeclaration[Pre]), Seq())) =>
      rewriteDefault(declaration)
    case declaration: CStructMemberDeclarator[Pre] =>
      declaration.decls.foreach(decl => {
        implicit val o: Origin = decl.o
        val (specs, newDecl) = normalizeCDeclaration(declaration.specs, decl, context = Some(declaration))
        cStructMemberDeclarators.declare(declaration.rewrite(
         specs = specs,
          decls = Seq(newDecl)
        ))
      })
    case declaration: CGlobalDeclaration[Pre] =>
      declaration.decl.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCDeclaration(declaration.decl.specs, init.decl, context = Some(declaration))
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
      val (specs, decl) = normalizeCDeclaration(declaration.specs, declaration.declarator, context = Some(declaration))
      globalDeclarations.declare(declaration.rewrite(specs = specs, declarator = decl))
    case param: CPPParam[Pre] =>
      val (specs, decl) = normalizeCPPDeclaration(param.specifiers, param.declarator, context = Some(param))(param.o)
      cPPParams.declare(new CPPParam(specs, decl)(param.o))
    case declaration: CPPLocalDeclaration[Pre] =>
      declaration.decl.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCPPDeclaration(declaration.decl.specs, init.decl, context = Some(declaration))
        cPPLocalDeclarations.declare(declaration.rewrite(
          decl = declaration.decl.rewrite(
            specs = specs,
            inits = Seq(
              CPPInit(decl, init.init.map(dispatch)),
            ),
          ),
        ))
      })
    case declaration: CPPGlobalDeclaration[Pre] =>
      declaration.decl.inits.foreach(init => {
        implicit val o: Origin = init.o
        val (specs, decl) = normalizeCPPDeclaration(declaration.decl.specs, init.decl, context = Some(declaration))
        globalDeclarations.declare(declaration.rewrite(
          decl = declaration.decl.rewrite(
            specs = specs,
            inits = Seq(
              CPPInit(decl, init.init.map(dispatch)),
            ),
          ),
        ))
      })
    case declaration: CPPFunctionDefinition[Pre] =>
      implicit val o: Origin = declaration.o
      val (specs, decl) = normalizeCPPDeclaration(declaration.specs, declaration.declarator, context = Some(declaration))
      globalDeclarations.declare(declaration.rewrite(specs = specs, declarator = decl))
    case cls: JavaClass[Pre] =>
      rewriteDefault(cls)
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case CDeclarationStatement(local) =>
      val (locals, _) = cLocalDeclarations.collect { dispatch(local) }
      Block(locals.map(CDeclarationStatement(_)(stat.o)))(stat.o)
    case CPPDeclarationStatement(local) =>
      val (locals, _) = cPPLocalDeclarations.collect { dispatch(local) }
      Block(locals.map(CPPDeclarationStatement(_)(stat.o)))(stat.o)

    case other => rewriteDefault(other)
  }
}
